let leq (ax, ay) (bx, by) = ax <= bx

# let main = mergesort (<=) [1,4,6,3,2,5,] & print

let main = splitby (\i-> i<5) (tabulate 10 identity) & print
