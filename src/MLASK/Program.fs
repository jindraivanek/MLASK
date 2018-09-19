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

let rewriteValsOcaml ast =
    [
        "op_Addition", "+"
        "op_Equality", "="
        "op_Inequality", "<>"
        "op_LessThan", "<"
        "op_Subtraction", "-"
        "op_ColonColon", "::"
        "op_PipeRight", "|>"

        "printfn", "(Printf.ksprintf print_endline)"
        "printf", "Printf.printf"
        "List.head", "List.hd"
        "List.tail", "List.tl"
    ] |> Seq.fold (fun ast (x,y) -> AstTransform.replaceVal x y ast) ast

let rewriteOcaml str = 
    str
    |> String.replace "op_Addition" "+"
    |> String.replace "op_Equality" "="
    |> String.replace "op_Inequality" "<>"
    |> String.replace "op_LessThan" "<"
    |> String.replace "op_Subtraction" "-"
    |> String.replace "op_ColonColon" "::"
    |> String.replace "op_PipeRight" "|>"

    |> String.replace "printfn " "(Printf.ksprintf print_endline) "
    |> String.replace "printf " "Printf.printf "
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

let compile transformF exprF rewrite ext inputs =
    System.IO.Directory.CreateDirectory outputDir
    let inputs = if List.isEmpty inputs then samples else inputs
    let withoutExt f =
        let n = System.IO.FileInfo(f).Extension.Length
        f.Substring(0, f.Length - n)
    inputs |> Seq.iter (fun fsFile ->
        let x = withoutExt fsFile
        printfn "%s" x
        let x = firstCharUpperCase x
        let outputPath = outputDir @@ System.IO.FileInfo(x).Name
        System.IO.File.WriteAllText(outputPath+".fsast", sprintf "%A" (MLASK.Inputs.FSharp.getFsAst fsFile))
        let ast = fsFile |> MLASK.Inputs.FSharp.toAST
        System.IO.File.WriteAllText(outputPath+".mlast", sprintf "%A" ast)
        let ast2 = ast |> transformF
        System.IO.File.WriteAllText(outputPath+".mlastt", sprintf "%A" ast2)
        let out = ast2 |> exprF |> rewrite 
        out |> printfn "%s"
        ast |> MLASK.AST.AstAnalyse.createBindsDict |> printfn "%A"
        System.IO.File.WriteAllText(outputPath+"."+ext, out))

let run inputs =
    try
        compile rewriteValsOcaml MLASK.Outputs.OCaml.getExpr id "ml" inputs
        // let transforms = 
        //     MLASK.AST.AstTransform.expandMatchLambda
        //     >> MLASK.AST.AstTransform.topLevelExprToMainFunction
        // compile transforms MLASK.Outputs.Haxe.getExprAndFormat rewriteHaxe "hx"
    with e -> printfn "%s::%s" e.Message e.StackTrace

[<EntryPoint>]
let main argv =
    run (Seq.toList argv)
    0 // return an integer exit code
