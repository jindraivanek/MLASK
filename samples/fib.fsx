let rec fib =
    function
    | x when x < 2 -> 1
    | x -> fib (x-1) + fib (x-2)

printf "%i" (fib 10)
