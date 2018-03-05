#' mvnpfd
#'
#' @param x
#' @param mean_vec
#' @param varcovM
#' @param log
#'
#' @return
#' @export
#'
#' @examples
mvnpfd <- function(x, mean_vec =rep(0, nrow(x)), varcovM=diag(nrow(x)), log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)

  mat_center <- x-(matrix(rep(mean_vec,p),ncol=n))
  vec_dens <- (1/(2*pi)^(p/2)*sqrt(det(varcovM))) * (exp(-0.5*t(mat_center) %*% solve(varcovM) %*% (mat_center)))

  return(list(x=x, vec_dens=vec_dens))
}

n <- 5
p <- 25
x <- matrix(rnorm(n*p, mean=0, sd=1), ncol=n)

print(vec_dens)
<<<<<<< HEAD
=======
print(n)
>>>>>>> 7e36b20defb8d236772534b78af010b7fa6144a1
print(p)
<<<<<<< HEAD
print(x)
=======

print("modif")
>>>>>>> 2c5cf80a0a70ede1d95323e103086af26a72f41a
