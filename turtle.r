turtle <- function(x, y, heading, stepsize, angle_increment) {

    x_orig <- x
    y_orig <- y
    heading_orig <- heading
    turtle_trace <- list(x1=c(x),x2=c(x),y1=c(y),y2=c(y))
    turtle_stack <- list(x=x,y=y,heading=heading)

    reset <- function() {
      x <<- x_orig
      y <<- y_orig
      turtle_trace <<- list(x1=c(x),x2=c(x),y1=c(y),y2=c(y))
      heading <<- heading_orig
    }

    forward <- function() {
      x <<- x + stepsize * cos(heading)
      y <<- y + stepsize * sin(heading)
    }

    forward_draw <- function() {
      turtle_trace$x1 <<- c(x, turtle_trace$x1)
      turtle_trace$y1 <<- c(y, turtle_trace$y1)
      forward()
      turtle_trace$x2 <<- c(x, turtle_trace$x2)
      turtle_trace$y2 <<- c(y, turtle_trace$y2)
    }

    turn_right <- function() {
      heading <<- heading - angle_increment
    }

    turn_left <- function() {
      heading <<- heading + angle_increment
    }

    draw_turtle <- function(ls) {
      print(c(range(c(turtle_trace$x1,turtle_trace$x2)),
              range(c(turtle_trace$y1,turtle_trace$y2))))

    plot(x=range(c(turtle_trace$x1,turtle_trace$x2)),
           y=range(c(turtle_trace$y1,turtle_trace$y2)),
           type="n", ann=FALSE, axes=FALSE)

      for (i in 1:length(turtle_trace$x1)) {
        lines(x=c(turtle_trace$x1[i], turtle_trace$x2[i]),
              y=c(turtle_trace$y1[i], turtle_trace$y2[i]))
      }
    }

    print_turtle_trace <- function() {
#      print(turtle_trace)
      print(heading*(180/pi))
    }

    push <- function() {
      turtle_stack[[length(turtle_stack) + 1]] <<-
        list(x = x, y = y, heading = heading)
    }

    pop <- function() {
      last_turtle <- turtle_stack[[length(turtle_stack)]]
      turtle_stack[[length(turtle_stack)]] <<- NULL
      x <<- last_turtle$x
      y <<- last_turtle$y
      heading <<- last_turtle$heading
    }

    function_table <-
      list("F" = forward_draw,
           "f" = forward,
           "-" = turn_right,
           "+" = turn_left,
           "n" = reset,
           "d" = draw_turtle,
           "p" = print_turtle_trace,
           "[" = push,
           "]" = pop)

    iter_over_nu <- function(nu) {
      for (i in 1:nchar(nu)) {
        a <- substring(nu,i,i)
        if (a %in% names(function_table)) {
          function_table[[a]]()
        }
      }
    }

    function(nu) {
      iter_over_nu(nu)
    }
  }
