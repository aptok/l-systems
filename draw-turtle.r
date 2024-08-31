draw_turtle <- function(ls) {
    if (length(ls$x1) == 0) {
        TRUE
    } else {
        lines(x=c(ls$x1[1], ls$x2[1]), y=c(ls$y1[1], ls$y2[1]))
        draw_lines(list(x1=ls$x1[-1],
                        x2=ls$x2[-1],
                        y1=ls$y1[-1],
                        y2=ls$y2[-1]))
    }
}


draw_turtle_iter <- function(ls) {
    for (i in 1:length(ls$x1)) {
        lines(x=c(ls$x1[i], ls$x2[i]), y=c(ls$y1[i], ls$y2[i]))
    }
}
