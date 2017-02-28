type a = { x: int; y: float; z: A }

let rec r = { x = 1; y = 2.0; z = r}

printf "%i" r.x

let r2 = { r with x = r.x + 1}

printf "%i" r2.x

let r3 = { r with z = { x = 0; y = 0.0; z = r2 } }

printf "%i %i" r3.x r3.z.x

let { y = x } = r in printf "%f" x

{{ x = 1; y = 2.0; z = r} with x = 3} |> (fun x -> x.x) |> printf "%i"
