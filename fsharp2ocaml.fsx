#load "IncludeMLASK.fsx"
open MLASK.AST

let (@@) x y = System.IO.Path.Combine(x, y) 
let samplesDir = "samples"
let outputDir = "output"
let samples = [
    "sample1"
    "fib"
    "curry"
    "multirec"
    "types"
    "record_bench"
]

let rewriteOcaml str = 
    str
    |> String.replace "op_Addition" "(+)"
    |> String.replace "op_Equality" "(=)"
    |> String.replace "op_Inequality" "(<>)"
    |> String.replace "op_LessThan" "(<)"
    |> String.replace "op_Subtraction" "(-)"
    |> String.replace "op_ColonColon" "(::)"
    |> String.replace "op_PipeRight" "(|>)"

    |> String.replace "printf" "Printf.printf"
    |> String.replace "List.head" "List.hd"
    |> String.replace "List.tail" "List.tl"

let rewriteHaxe str = 
    str
    |> String.replace "op_Addition" "+"
    |> String.replace "op_Equality" "=="
    |> String.replace "op_Inequality" "!="
    |> String.replace "op_LessThan" "<"
    |> String.replace "op_Subtraction" "-"
    |> String.replace "op_ColonColon" "::"
    |> String.replace "op_PipeRight" "|>"

    |> String.replace "printfn(\"%A\")" "Sys.println"
    |> String.replace "printfn(\"%i\")" "Sys.println"
    |> String.replace "printfn(\"%s\")" "Sys.println"
    |> String.replace "printfn" "Sys.println"
    |> String.replace "printf(\"%A\")" "Sys.print"
    |> String.replace "printf(\"%i\")" "Sys.print"
    |> String.replace "printf(\"%s\")" "Sys.print"
    |> String.replace "printf" "Sys.print"
    |> String.replace "List.head" "List.hd"
    |> String.replace "List.tail" "List.tl"
    

let firstCharUpperCase s = s |> String.mapi (fun i c -> if i = 0 then System.Char.ToUpper c else c)

let compile transformF exprF rewrite ext =
    samples |> Seq.iter (fun x ->
        let fsFile = samplesDir @@ x + ".fsx" 
        let x = firstCharUpperCase x
        let outputPath = outputDir @@ x
        System.IO.File.WriteAllText(outputPath+".fsast", sprintf "%A" (FSharpInput.getFsAst fsFile))
        let ast = fsFile |> FSharpInput.toAST
        System.IO.File.WriteAllText(outputPath+".mlast", sprintf "%A" ast)
        let ast2 = ast |> transformF
        System.IO.File.WriteAllText(outputPath+".mlastt", sprintf "%A" ast2)
        let out = ast2 |> exprF |> rewrite 
        out |> printfn "%s"
        ast |> MLASK.AST.AstAnalyse.createBindsDict |> printfn "%A"
        System.IO.File.WriteAllText(outputPath+"."+ext, out))

#if INTERACTIVE
#else
let run() =
#endif
try
    //compile OcamlOutput.getExpr rewriteOcaml "ml"
    let transforms = 
        MLASK.AST.AstTransform.expandMatchLambda
        >> MLASK.AST.AstTransform.topLevelExprToMainFunction
    compile transforms HaxeOutput.getExprAndFormat rewriteHaxe "hx"
with e -> printfn "%s::%s" e.Message e.StackTrace