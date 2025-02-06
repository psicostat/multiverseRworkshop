devtools::load_all()
library(tidyverse)
library(MASS)

select <- dplyr::select

# number of times phd students says "**** off I quit!"
# coffee consumption per day
# alcool consumption per week
# average number of hours of sleep
# number of weekly work hours
# netflix hours per week
# netflix genre
#   teen drama, crime, anime, comedy
# lab drama score
#   from 0 to 50
# statistical package used
#   R, SPSS, Excel

set.seed(6393)

N <- 100

coffee <- rpois(N, 3)
alcool <- c(rpois(N*0.8, 5), rep(0, N*0.2))
sleep  <- rpois(N, 6)
whours <- round(runif(N, 20, 80))
g_netflix <- sample(c("teen_drama", "kdrama", "crime", "anime", "comedy"), N, replace = TRUE)
h_netflix <- rpois(N, 5)
lab_drama <- trim(round(rnorm(N, 30, 10)), 0, 50)
software <- sample(c("r", "spss", "excel"), N, replace = TRUE)

phd <- data.frame(
    coffee,
    alcool,
    sleep,
    whours,
    g_netflix,
    h_netflix,
    lab_drama,
    software
)

X <- model.matrix(~ coffee + alcool + sleep + whours + g_netflix + h_netflix + lab_drama + software, data = phd)
b0 <- log(5)
B <- c(b0, log(1), log(runif(ncol(X) - 2,0.95, 1.05)))
lp <- c(X %*% B)
phd$phd_curse <- rnb(N, exp(lp), vmr = 2)

polyn <- function(x, degree = 1){
    function(x) poly(x, degree = degree)
}

cutN <- function(x, breaks = 2){
    function(x){
        cut(x, breaks = breaks)
    }
}

slog <- function(x){ # safe version of log()
    if(any(x == 0)){
        x <- x + 1
    }
    log(x)
}

cut2 <- cutN(breaks = 2)
cut4 <- cutN(breaks = 4)
poly2 <- polyn(degree = 2)
poly3 <- polyn(degree = 3)


multi <- multiverse(formula = phd_curse ~ coffee + alcool + software,
                    nfuns = c("slog", "cut2", "cut4", "poly2", "poly3"),
                    focal = "coffee",
                    data = phd,
                    model = list(gaussian = c("identity"),
                                 poisson = "log",
                                 quasipoisson = "log"),
                    fit = TRUE,
                    remove = c(10, 15, 20, 30, 40)
)

mods <- multi$mods

mods_tidy <- vector(mode = "list", length = length(mods))

for(i in 1:length(mods)){
    if(inherits(mods[[i]], "glm")){
        if(mods[[i]]$family$link == "log"){
            mods_tidy[[i]] <- broom::tidy(mods[[i]], exponentiate = FALSE)
        } else{
            mods_tidy[[i]] <- broom::tidy(mods[[i]])
        }
    } else{
        mods_tidy[[i]] <- broom::tidy(mods[[i]])
    }
}

mods_tidy <- dplyr::bind_rows(mods_tidy, .id = "mod")
mods_tidy <- filter(mods_tidy, term == "coffee")
beta <- mods_tidy$estimate
p.value <- mods_tidy$p.value


dat <- data.frame(beta, p.value)
dat$p.value <- ifelse(dat$p.value <= 10^(-digits), 10^(-digits), dat$p.value)
if(ptransf == "none"){
    ylab <- "p-values"
    dat$p.transf <- dat$p.value
} else if(ptransf == "z"){
    ylab <- "qnorm(1 - p/2)"
    dat$p.transf <- qnorm(1 - dat$p.value/2)
} else if(ptransf == "-log10"){
    ylab <- "-log10(p)"
    dat$p.transf <- -log10(p)
}

dat$sign <- dat$p.value <= 0.05
p <- ggplot(data = dat,
       aes(x = beta,
           y = p.transf,
           color = sign)) +
    geom_point()
plot(p)

