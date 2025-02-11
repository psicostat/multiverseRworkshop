make_id <- function(x){
    as.integer(factor(x))
}

trim <- function(x, min, max){
    x[x <= min] <- min
    x[x >= max] <- max
    return(x)
}

make_call <- function(x, FUN){
    deparse(call(FUN, as.name(x)))
}

sanitize_list <- function(x = NULL, names = NULL){
    if(is.null(x) & is.null(names)){
        x <- NULL
    } else{
        if(is.null(x)){
            x <- as.list(rep("identity", length(x)))
            names(x) <- names
        } else{
            x <- lapply(x, function(e) c("identity", e))
            num_missing <- names[!names %in% names(x)]
            x <- c(
                list(rep("identity", length(num_missing))),
                x
            )
            names(x) <- c(num_missing, names(x)[2:length(x)])
        }
    }
    return(x)
}

fac2char <- function(x){
    isfactor <- sapply(x, is.factor)
    x <- lapply(x, function(c) if(is.factor(c)) as.character(c) else c)
    data.frame(x)
}

is_number <- function(x){
    is.numeric(x)
}

#' Create the scenarios for the multiverse
#'
#' @param formula a formula describing the maximal model with bare variables (no tranformations). Currently not supporting interactions.
#' @param focal optional name of the coefficient that is the focus of the analysis. No transformation will be applied to that variable and no model will exclude this variable.
#' @param nfuns functions to be applied to numerical variables. functions need to be provided as characters. this argument can be a vector of functions (e.g., `c("log", "exp")`) and the functions will be applied al all numerical variable exluding the focal predictor. In alternative, can be a named list specifing the name of the variable and the functions as string (e.g., `list(x = "log", z = c("exp"))`) in this way the functions are variable-specific. If `NULL` no transformations will be applied.
#' @param cfuns same as the `nfuns` but for factor/character variables. If `NULL` no transformations will be applied.
#' @param data the dataset for the model
#'
#' @return a list
#' @export
#'
create_multi <- function(formula,
                         focal = NULL,
                         nfuns = NULL,
                         cfuns = NULL,
                         data){
    xs <- formula.tools::rhs.vars(formula)
    xs_type <- sapply(data, class)[xs]

    xs_num <- xs[xs_type == "numeric" | xs_type == "integer"]
    xs_chr <- xs[!(xs_type == "numeric" | xs_type == "integer")]

    xs_num <- if(length(xs_num) == 0) NULL else xs_num
    xs_chr <- if(length(xs_chr) == 0) NULL else xs_chr

    xx <- as.list(rep("identity", length(xs)))
    names(xx) <- xs

    xx <- stack(xx)
    xx <- fac2char(xx)

    if(length(xs_num) != 0){
        if(!is.list(nfuns)){
            if(!is.null(nfuns)){
                nfuns <- rep(list(nfuns), length(xs_num))
                names(nfuns) <- xs_num
                nfuns <- stack(nfuns)
            }
        }
        xx <- rbind(xx, nfuns)
    }

    if(length(xs_chr) != 0){
        if(!is.list(cfuns)){
            if(!is.null(cfuns)){
                cfuns <- rep(list(cfuns), length(xs_chr))
                names(cfuns) <- xs_chr
                cfuns <- stack(cfuns)
            }
        }
        xx <- rbind(xx, cfuns)
    }

    rownames(xx) <- NULL

    xx$type <- xs_type[xx$ind]
    X <- xx
    names(X) <- c("fun", "x", "type")
    X <- fac2char(X)

    X$focal <- FALSE
    X$focal[X$x == focal] <- TRUE

    if(!is.null(focal)){
        X$fun <- ifelse(X$x == focal & X$fun != "identity", "identity", X$fun)
    }

    X$.id_fun <- make_id(X$fun)
    X$.id_x <- make_id(X$x)

    X <- X[!duplicated(X), ]

    X$call <- NA
    for(i in 1:nrow(X)){
        if(X$fun[i] == "identity"){
            X$call[i] <- X$x[i]
        }else{
            X$call[i] <- make_call(X$x[i], X$fun[i])
        }
    }

    xu <- X$call
    names(xu) <- X$x
    forms <- sapply(1:length(xu), function(i) combn(xu, i, simplify = FALSE))
    forms <- unlist(forms, recursive = FALSE)
    dup <- sapply(forms, function(x) length(unique(names(x))) != length(names(x)))
    forms <- forms[!dup]
    # remove calls without the focal predictor
    if(!is.null(focal)){
        has_focal <- sapply(forms, function(x) any(x == focal))
        forms <- forms[has_focal]
    }

    forms_call <- lapply(forms, paste, collapse = " + ")
    forms_call <- paste("~", forms_call)

    # # check if the marginality principle is respected
    # forms_as_formula <- sapply(forms_call, as.formula)
    # is_marginal <- sapply(forms_as_formula, function(x) check_marginality(x))
    # forms_call <- forms_call[!is_marginal]
    out <- list(X = X, calls = unlist(forms_call))
    return(out)
}


glm_fun <- function(model, link = NULL){
    if(model == "poisson"){
        fam <- if(is.null(link)) poisson() else poisson(link = link)
        function(formula, data) glm(formula, data = data, family = fam)
    } else if(model == "negbin"){
        function(formula, data) MASS::glm.nb(formula, data = data)
    } else if(model == "gaussian"){
        function(formula, data) lm(formula, data = data)
    } else if(model == "quasipoisson"){
        fam <- if(is.null(link)) quasipoisson() else quasipoisson(link = link)
        function(formula, data) glm(formula, data = data, family = fam)
    } else if(model == "binomial"){
        fam <- if(is.null(link)) binomial() else binomial(link = link)
        function(formula, data) glm(formula, data = data, family = fam)
    } else{
        msg <- sprintf("Method '%s' not implemented yet!", model)
        stop(msg)
    }
}

remove_obs <- function(x, rows){
    x[-rows, ]
}

which.uni.outliers <- function(x,
                               method = c("iqr", "z", "perc", "mad"),
                               cr){
    # https://statsandr.com/blog/outliers-detection-in-r/#fn4
    if(method == "iqr"){
        iqr <- stats::IQR(x)
        q25 <- quantile(x, 0.25)
        q75 <- quantile(x, 0.75)
        is_out <- x < (q25 - iqr * cr) | x > (q75 + iqr * cr)
    } else if(method == "z"){
        xz <- scale(x)[, 1]
        is_out <- abs(xz) > cr
    } else if(method == "perc"){
        # th is the % of coverage
        alpha <- (1 - cr)
        lb <- quantile(x, alpha/2)
        ub <- quantile(x, 1 - alpha/2)
        is_out <- x < lb | x > ub
    } else if(method == "mad"){
        mad <- mad(x, constant = 1)
        lb <- median(x) - cr * mad
        ub <- median(x) + cr * mad
        is_out <- x < lb | x > ub
    } else{
        stop("method not implemented!")
    }
    which(is_out)
}

which.multi.outliers <- function(formula, data, method = c("cook", "mahalanobis"), cr = NULL){
    if(method == "cook"){
        if(is.null(cr)) stop("cr need to be provided with cook")
        fit <- lm(formula, data = data)
        cc <- cooks.distance(fit)
        is_out <- unname(cc > mean(cc) * cr)
    } else if(method == "mahalanobis"){
        # https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/mahal#:~:text=The%20Mahalanobis%20distance%20(MD)%20on,(1990)%20and%20here).&text=If%20MD(i)%20is%20greater,observation%20i%20is%20an%20outlier.

        xs <- formula.tools::rhs.vars(formula)
        ys <- formula.tools::lhs.vars(formula)
        vv <- c(xs, ys)
        dd <- data[, vv]
        cc <- colMeans(dd)
        vv <- cov(dd)
        di <- mahalanobis(dd, cc, vv)
        cr <- qchisq(0.999, length(vv))
        is_out <- di > cr
    }
    which(is_out)
}

check_marginality <- function(formula, except = NULL) {
    # taken from
    # # https://github.com/cran/MuMIn/blob/1834bb90bade3912317a15c9b7c19771f19cf6dc/R/formulas.R#L2
    factors <- attr(terms.formula(formula, simplify = FALSE), "factors")
    if(length(factors) == 0L) return(TRUE)
    ex <- dimnames(factors)[[1L]][rowSums(factors > 1L) != 0L]
    if(is.character(except))
        factors <- factors[!(dimnames(factors)[[1L]] %in% except), ]
    ret <- all(factors < 2L)
    attr(ret, "marg.ex") <- ex
    return(ret)
}

expand_formula <- function(x, combine = FALSE){
    # ys <- formula.tools::lhs.vars(x)
    # xs <- formula.tools::rhs.vars(x)
    terms <- attr(terms.formula(x), "term.labels")
    if(combine){
        paste(terms, collapse = " + ")
    } else{
        terms
    }
}




multiverse <- function(formula,
                       data,
                       focal = NULL,
                       nfuns = NULL,
                       cfuns = NULL,
                       model = NULL,
                       fit = FALSE,
                       remove = NULL){

    # creating all the models

    multi <- create_multi(formula = formula,
                          focal = focal,
                          nfuns = nfuns,
                          cfuns = cfuns,
                          data = data)

    # creating the combinations between calls and models with
    # link function

    glms <- stack(model)[, c(2,1)] # stack and revert column order
    names(glms) <- c("family", "link")

    cc <- rep(multi$calls, each = nrow(glms))

    idx <- rep(1:nrow(glms), each = length(multi$calls))
    grid <- glms[idx, ]
    grid$call <- rep(multi$calls, nrow(glms))

    if(!is.null(remove)){
        old <- grid
        idx <- rep(1:nrow(grid), each = 2)
        grid <- old[idx, ]
        grid$remove <- rep(c(TRUE, FALSE), nrow(old))
    } else{
        grid$remove <- FALSE
    }

    grid$model <- paste0("m", 1:nrow(grid))
    rownames(grid) <- NULL

    ys <- formula.tools::lhs.vars(formula)
    out <- list()

    # fitting the models
    if(fit){
        mods <- vector(mode = "list", length = nrow(grid))
        for(i in 1:nrow(grid)){
            if(grid$remove[i]){
                tdata <- data[-remove, ]
            } else{
                tdata <- data
            }
            fit_fun <- with(grid[i, ], glm_fun(family, link))
            form <- paste(ys, grid$call[i])
            mods[[i]] <- fit_fun(form, tdata)
        }
        names(mods) <- grid$model
        out$mods <- mods
    }

    out$multi <- multi
    out$grid <- grid
    out
}

get_info_models <- function(x){
    x$X <- tibble(x$X)
    focal <- x$X$x[x$X$focal]
    names(x$X$x) <- x$X$call
    allx <- sapply(multi$calls, function(cc) formula.tools::rhs.vars(as.formula(cc)))

    ll <- length(unique(x$X$x))

    allx <- lapply(allx, function(x) {
        if(length(x) < ll){
            x <- c(x, rep(NA, ll - length(x)))
            x
        } else{
            x
        }
    })

    allx <- t(data.frame(allx))
    rownames(allx) <- NULL
    allx <- data.frame(allx)

    # allx |>
    #     mutate(mod = 1:n()) |>
    #     pivot_longer(-mod) |>
    #     mutate(x = x$X$x[value]) |>
    #     select(-name) |>
    #     mutate(x = paste0("x_", x)) |>
    #     pivot_wider(names_from = x, values_from = value)
    allx |>
        mutate(mod = 1:n()) |>
        pivot_longer(-mod) |>
        mutate(x = x$X$x[value]) |>
        select(-name) |>
        drop_na() |>
        mutate(x = paste0("x_", x)) |>
        pivot_wider(names_from = x, values_from = value)
}



expl_var <- function(x, tot = "explained"){
    aov <- broom::tidy(car::Anova(x))
    if(tot == "explained"){
        idx <- 1:(nrow(aov) - 1)
    } else{
        idx <- 1:nrow(aov)
    }
    tt <- sum(aov$sumsq[idx])
    data.frame(
        term = aov$term[idx],
        eta2 = aov$sumsq[idx] / tt
    )
}

simMulti <- function(ns, m = 0, p, r, data = FALSE){
    if(length(m) == 1){
        m <- rep(m, p)
    }
    R <- r + diag(1 - r, p)
    df <- ns - 1
    V <- rWishart(n = 1, df = df, Sigma = R)
    Rhat <- cov2cor(V[, , 1])
    X <- MASS::mvrnorm(ns, m, Rhat)
    if(data){
        data.frame(X)
    } else{
        apply(X, 2, function(x) t.test(x)$p.value)
    }
}
