filor::try_seed({
    set.seed(8883)
    P <- 5
    N <- 50
    R <- filor::rmat(runif(filor::ncor(P), 0, 0.6))
    R <- Matrix::nearPD(R)$mat
    X <- MASS::mvrnorm(N, rep(0, P), R)
    B <- c(0, rep(0, P))
    X <- data.frame(X)
    form <- sprintf("~ %s", paste0(names(X), collapse = " + "))
    XM <- model.matrix(as.formula(form), data = X)
    y <- rnorm(N, c(XM %*% B), 1)
    multi <- create_multi(as.formula(form), data = X)

    fitl <- vector(mode = "list", length = length(multi$calls))

    for(j in 1:length(multi$calls)){
        ff <- paste0("y ", multi$calls[j])
        fitl[[j]] <- lm(ff, data = X)
    }

    res <- lapply(fitl, get_coef)
    res <- bind_rows(res, .id = "mod")
    names(res) <- c("mod", "b", "se", "t", "p", "param")
    pp <- res |>
        filter(param != "(Intercept)") |>
        mutate(sign = p <= 0.05) |>
        ggplot(aes(x = b, y = -log10(p))) +
        geom_point(aes(shape = sign, color = param))

    plot(pp)
})

1497
7987
8883


get_coef <- function(x, coef = NULL){
    xs <- data.frame(summary(x)$coefficients)
    if(!is.null(coef)){
        xs <- xs[coef, ]
    }
    xs$param <- rownames(xs)
    return(xs)
}

xx <- vector(mode = "list", length = 1e3)

for(i in 1:1e3){
    P <- 5
    N <- 100
    R <- filor::rmat(runif(filor::ncor(P), 0, 0.6))
    R <- Matrix::nearPD(R)$mat
    X <- MASS::mvrnorm(N, rep(0, P), R)
    B <- c(0, rep(0, P))
    X <- data.frame(X)
    form <- sprintf("~ %s", paste0(names(X), collapse = " + "))
    XM <- model.matrix(as.formula(form), data = X)
    y <- rnorm(N, c(XM %*% B), 1)
    multi <- create_multi(as.formula(form), data = X)

    fitl <- vector(mode = "list", length = length(multi$calls))

    for(j in 1:length(multi$calls)){
        ff <- paste0("y ", multi$calls[j])
        fitl[[j]] <- lm(ff, data = X)
    }

    res <- lapply(fitl, get_coef)
    res <- bind_rows(res, .id = "mod")
    names(res) <- c("mod", "b", "se", "t", "p", "param")
    xx[[i]] <- res
}

xxd <- bind_rows(xx, .id = "sim")
rownames(xxd) <- NULL

xxd |>
    filter(param != "(Intercept)") |>
    group_by(sim) |>
    summarise(is_sign = any(p <= 0.05)) |>
    summarise(type1 = mean(is_sign))
