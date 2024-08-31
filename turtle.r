turtle <- function(x, y, alpha, stepsize, delta) {

  x_orig <- x
  y_orig <- y
  alpha_orig <- alpha

  reset <- function() {
    linesxy <<- list(x1=c(),x2=c(),y1=c(),y2=c())
    x <<- x_orig
    y <<- y_orig
    alpha <<- alpha_orig
  }

  forward <- function() {
    x <<- x + stepsize * cos(alpha)
    y <<- y + stepsize * sin(alpha)
  }

  forward_draw <- function() {
    linesxy$x1 <<- c(x, linesxy$x1)
    linesxy$y1 <<- c(y, linesxy$y1)
    forward()
    linesxy$x2 <<- c(x, linesxy$x2)
    linesxy$y2 <<- c(y, linesxy$y2)
  }

  turn_right <- function() {
    alpha <<- alpha - delta
  }

  turn_left <- function() {
    alpha <<- alpha + delta
  }

  linesxy <- list(x1=c(),x2=c(),y1=c(),y2=c())

  function_table <-
    list("F" = forward_draw,
         "f" = forward,
         "-" = turn_right,
         "+" = turn_left,
         "n" = reset)

  rec_over_nu <- function(nu) {
    a <- substring(nu,1,1)
    if (nu == "") {
      linesxy
    } else {
      if (a %in% names(function_table)) {
        function_table[[a]]()
      } 
      rec_over_nu(substring(nu,2))
    }
  }

  iter_over_nu <- function(nu) {
    for (i in 1:nchar(nu)) {
      a <- substring(nu,i,i)
      if (a %in% names(function_table)) {
        function_table[[a]]()
      }
    }
    linesxy
  }

  function(nu) {
    iter_over_nu(nu)
  }
}
