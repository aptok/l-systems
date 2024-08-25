turtle <- function(x, y, alpha, stepsize, delta) {

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
             "+" = turn_left)

    rec_over_nu <- function(nu) {
        if (nu == "") {
            linesxy
        } else {
            function_table[[substring(nu,1,1)]]()
            rec_over_nu(substring(nu,2))
        }
    }

    iter_over_nu <- function(nu) {
        while (nu != "") {
            function_table[[substring(nu,1,1)]]()
            nu <- substring(nu,2)
        }
        linesxy
    }

    function(nu) {
                                        # rec_over_nu(nu)
        iter_over_nu(nu)
    }
}
