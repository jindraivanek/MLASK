let foo() = 
    let msg = "Hello world"
    let x = 1
    let y = x + 2
    printfn "%i" y
    if y<3 then
      printfn "%s" msg
    else
      printfn "baf"
      printfn "%s" msg
let main() =
  foo()