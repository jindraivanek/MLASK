let foo() = 
    let msg = "Hello world"
    let x = 1
    let y = x + 2
    printf "%i" y
    if y=3 then 
      printf "%s" msg
foo()