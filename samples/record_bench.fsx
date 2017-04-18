type r = { x : int; y : int }

let rec create = 
    function
    | 0 -> []
    | n -> { x = n; y = n} :: create (n-1)

let rec step i =
    function
    | [] -> i
    | (x::xs) -> 
        xs |> List.map (fun {x=x;y=y} -> {x=x+1; y=y+1}) 
        |> List.filter (fun t -> x <> t)
        |> step (i+1)

printf "%i" (step 0 (create 10000))