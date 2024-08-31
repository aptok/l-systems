lsystem_rec  <- function(alphabet, axiom, productions) {

  derivation  <- function(axiom, new_word) {
    a <- substring(axiom,1,1)
    if (a  == "") {
      new_word
    } else derivation(substring(axiom,2),
                      paste(new_word,
                            productions[a],
                            sep=""))
  }

  derive_n  <- function(axiom, n) {
    if (n == 0) {
      axiom
    } else derive_n(derivation(axiom, ""),
                    n - 1)
  }

  function(n) {
    derive_n(axiom, n)
  }
}

turtle <- function(x, y, alpha, stepsize, delta) {

  x_orig <- x
  y_orig <- y
  alpha_orig <- alpha
  ret <- list(x1=c(),x2=c(),y1=c(),y2=c())

  reset <- function() {
    ret <<- list(x1=c(),x2=c(),y1=c(),y2=c())
    x <<- x_orig
    y <<- y_orig
    alpha <<- alpha_orig
  }

  forward <- function() {
    x <<- x + stepsize * cos(alpha)
    y <<- y + stepsize * sin(alpha)
  }

  forward_draw <- function() {
    ret$x1 <<- c(x, ret$x1)
    ret$y1 <<- c(y, ret$y1)
    forward()
    ret$x2 <<- c(x, ret$x2)
    ret$y2 <<- c(y, ret$y2)
  }

  turn_right <- function() {
    alpha <<- alpha - delta
  }

  turn_left <- function() {
    alpha <<- alpha + delta
  }

  function_table <-
    list("F" = forward_draw,
         "f" = forward,
         "-" = turn_right,
         "+" = turn_left,
         "n" = reset)

  rec_over_nu <- function(nu) {
    a <- substring(nu,1,1)
    if (nu == "") {
      ret
    } else {
      if (a %in% names(function_table)) {
        function_table[[a]]()
      } 
      rec_over_nu(substring(nu,2))
    }
  }

  function(nu) {
    rec_over_nu(nu)
  }
}

draw_turtle <- function(name, turtle, lsystem, n) {
  ls <- turtle(lsystem(n))
  turtle("n")
  draw_turtle_rec <- function(ls) {
    if (length(ls$x1) == 0) {
      return()
    } else {
      lines(x=c(ls$x1[1], ls$x2[1]), y=c(ls$y1[1], ls$y2[1]))
      draw_turtle_rec(list(x1=ls$x1[-1],
                       x2=ls$x2[-1],
                       y1=ls$y1[-1],
                       y2=ls$y2[-1]))
    } 
  }
  png(name)
  plot(range(c(ls$x1, ls$x2)), range(c(ls$y1, ls$y2)), type="n", ann=FALSE, axes=FALSE)
  draw_turtle_rec(ls)
  dev.off()
}
