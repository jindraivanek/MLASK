#load "Include.fsx"
open MLASK.AST

let (@@) x y = System.IO.Path.Combine(x, y) 
let samplesDir = "samples"
let outputDir = "output"
let samples = [
    "sample1"
    "fib"
    "curry"
    "multirec"
    "problems99"
    "types"
]

let rec rewriteAst =
    function
    | ExprVal(ValId("printfn")) -> ExprVal(ValId("Printf.printf"))
    | x -> x

let rewrite str = 
    str
    |> String.replace "op_Addition" "(+)"
    |> String.replace "op_Equality" "(=)"
    |> String.replace "op_LessThan" "(<)"
    |> String.replace "op_Subtraction" "(-)"
    |> String.replace "op_ColonColon" "(::)"
    |> String.replace "op_PipeRight" "(|>)"

    |> String.replace "printf" "Printf.printf"
    |> String.replace "List.head" "List.hd"
    |> String.replace "List.tail" "List.tl"
    

let compile () =
    samples |> Seq.iter (fun x ->
        let fsFile = samplesDir @@ x + ".fsx" 
        let outputPath = outputDir @@ x
        System.IO.File.WriteAllText(outputPath+".fsast", sprintf "%A" (FSharpInput.getFsAst fsFile))
        let ast = fsFile |> FSharpInput.toAST
        System.IO.File.WriteAllText(outputPath+".mlast", sprintf "%A" ast)
        let out = ast |> OcamlOutput.getExpr |> rewrite 
        out |> printfn "%s"
        System.IO.File.WriteAllText(outputPath+".ml", out))

try
    compile()
with e -> printfn "%s" e.Message