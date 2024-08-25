source("lsystem.r")
source("turtle.r")
source("draw-turtle.r")
dict <- c("F" = "F-F+F+FF-F-F+F", "+" = "+", "-" = "-", "f" = "f")
plot.new()
draw_turtle_iter(
    turtle(0.5,0.5,pi,.006,0.5*pi)
    (lsystem_iter("Ff+-", "F-F-F-F", dict)
        (2)))
