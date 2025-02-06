rnb <- function(n, mu, vmr = NULL, theta = NULL, message = FALSE){
  if(is.null(theta) & is.null(vmr)){
    stop("theta or vmr need to be specified!")
  }
  if(!is.null(vmr)){
    if(vmr == 1){
      msg <- sprintf("y ~ Poisson(mu = %2.f), vmr = %.2f", mu, vmr)
      y <- rpois(n, mu)
    } else{
      res <- theta_from_vmr(mu, vmr)
      y <- MASS::rnegbin(n, mu, res$theta)
      msg <- sprintf("y ~ NegBin(mu = %2.f, theta = %.2f), var = %.2f, vmr = %.2f", mu, res$theta, res$v, vmr)
    }
  } else{
    res <- vmr_from_theta(mu, theta)
    y <- MASS::rnegbin(n, mu, res$theta)
    msg <- sprintf("y ~ NegBin(mu = %2.f, theta = %.2f), var = %.2f, vmr = %.2f", mu, theta, res$v, res$vmr)
  }
  if(message) message(msg)
  return(y)
}

theta_from_vmr <- function(mu, vmr){
    # vmr = v / m
    v <- mu * vmr
    # v = mu + mu^2/phi
    theta <- -(mu^2/(mu - v))
    if(vmr == 1){
      warning("when vmr = 1, theta = -Inf")
    }
    list(theta = theta, v = v, vmr = vmr)
}

vmr_from_theta <- function(mu, theta){
  v <- mu + mu^2/theta
  vmr <- v/mu
  list(theta = theta, v = v, vmr = vmr)
}