let rec f =
    function
    | 0 -> 0 
    | x -> 2 + g (x - 1)
and g =
    function
    | 0 -> 0 
    | x -> 1 + f (x - 1)

printf "%i" (f 10)