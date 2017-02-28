type A = { x: int; y: float; z: A }

let rec r = { x = 1; y = 2.0; z = r}

printf "%i" r.x