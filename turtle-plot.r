turtle_plot_png <- function(name, n, lsystem, turtle) {
  turtle_trace  <- turtle(lsystem(n))
  turtle("n")
  rx <- range(c(turtle_trace$x1,turtle_trace$x2))
  ry <- range(c(turtle_trace$y1,turtle_trace$y2))
  png(name)
  plot(c(rx[1], rx[2]),
       c(ry[1], ry[2]),
       type="n",
       ann=FALSE,
       axes=FALSE)
  draw_turtle_iter(turtle_trace)
  dev.off()
}
