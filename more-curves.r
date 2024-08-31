source("lsystem-with-subscripts.r")
source("turtle.r")
source("draw-turtle.r")
dict <- c("F" = "FF-F-F-F-F-F+F", "+" = "+", "-" = "-", "f" = "f")
l <- (lsystem_iter("Ff+-", "F-F-F-F", dict) (4))
t <- turtle(0,0,pi,1,pi/2)
ls <- t(l)
png("more-curves-a.png", width=1000, height=1000)
plot(x=range(x=c(ls$x1,ls$x2)),
     y=range(c(ls$y1,ls$y2)),
     type="n", ann=FALSE, axes=FALSE)
draw_turtle_iter(ls)
dev.off()
