draw_turtle <- function(lines) {
    if (length(lines$x1) == 0) {
        TRUE
    } else {
        lines(x=c(lines$x1[1], lines$x2[1]), y=c(lines$y1[1], lines$y2[1]))
        draw_lines(list(x1=lines$x1[-1],
                        x2=lines$x2[-1],
                        y1=lines$y1[-1],
                        y2=lines$y2[-1]))
    }
}


draw_turtle_iter <- function(lines) {
    for (i in 1:length(lines$x1)) {
        lines(x=c(lines$x1[i], lines$x2[i]), y=c(lines$y1[i], lines$y2[i]))
    }
}
