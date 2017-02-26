#load "Include.fsx"
open MLASK.AST

let (@@) x y = System.IO.Path.Combine(x, y) 
let samplesDir = "samples"
let outputDir = "output"
let samples = [
    "sample1"
    "fib"
    "curry"
]

let rec rewriteAst =
    function
    | ExprVal(ValId("printfn")) -> ExprVal(ValId("Printf.printf"))
    | x -> x

let rewrite str = 
    str
    |> String.replace "printf" "Printf.printf"
    |> String.replace "op_Addition" "(+)"
    |> String.replace "op_Equality" "(=)"
    |> String.replace "op_LessThan" "(<)"
    |> String.replace "op_Subtraction" "(-)"

samples |> Seq.iter (fun x ->
    let fsFile = samplesDir @@ x + ".fsx" 
    let outputPath = outputDir @@ x
    System.IO.File.WriteAllText(outputPath+".fsast", sprintf "%A" (FSharpInput.getFsAst fsFile))
    let ast = fsFile |> FSharpInput.toAST
    System.IO.File.WriteAllText(outputPath+".mlast", sprintf "%A" ast)
    let out = ast |> OcamlOutput.getExpr |> rewrite 
    out |> printfn "%s"
    System.IO.File.WriteAllText(outputPath+".ml", out))