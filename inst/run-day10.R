x <- readLines("./inst/input10.txt")

(p1 <- solve10a(x))
(p2 <- solve10b(x))

stopifnot(p1 == aoc_solutions$day10a)
stopifnot(p2 == aoc_solutions$day10b)
