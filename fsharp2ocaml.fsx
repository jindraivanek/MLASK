#load "Include.fsx"
open MLASK.AST

let rec rewriteAst =
    function
    | ExprVal(ValId("printfn")) -> ExprVal(ValId("Printf.printf"))
    | x -> x

let rewrite str = 
    str
    |> String.replace "printf" "Printf.printf"
    |> String.replace "op_Addition" "(+)"
    |> String.replace "op_Equality" "(=)"

let ast = "samples/sample1.fsx" |> FSharpInput.toAST 
ast |> printfn "%A"
let out = ast |> OcamlOutput.getExpr |> rewrite 
out |> printfn "%s"
System.IO.File.WriteAllText("output/sample1.ml", out)