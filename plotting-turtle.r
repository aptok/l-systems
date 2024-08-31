plotting_turtle <- function(x, y, alpha, stepsize, delta) {

    forward <- function() {
        x <<- x + stepsize * cos(alpha)
        y <<- y + stepsize * sin(alpha)
    }

    forward_draw <- function() {
        x1 <- x
        y1 <- y
        x <<- x + stepsize * cos(alpha)
        y <<- y + stepsize * sin(alpha)
        lines(c(x1,x),c(y1,y))
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
             "+" = turn_left)

    iter_over_nu <- function(nu) {

        for (i in 1:nchar(nu)) {
            function_table[[substring(nu,i,i)]]()
        }
    }

    function(nu) {
        iter_over_nu(nu)
    }
}
