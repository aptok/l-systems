source("lsystem.r")
source("turtle.r")
source("draw-turtle.r")
dict <- c("F" = "FF-F-F-F-F-F+F", "+" = "+", "-" = "-", "f" = "f")
l <- (lsystem_iter("Ff+-", "F-F-F-F", dict) (4))
t <- turtle(0.8,0.2,pi,0.3,pi/2)
lines <- t(l)
png("more-curves-a.png", width=5000, height=5000)
plot(x=range(x=c(lines$x1,lines$x2)),
     y=range(c(lines$y1,lines$y2)),
     type="n", ann=FALSE, axes=FALSE)
draw_turtle_iter(lines)
dev.off()
