#load "IncludeMLASK.fsx"

open MLASK.AST

let nl = System.Environment.NewLine

let surround head tail body = head + body + tail

let delim sep xs = xs |> String.concat sep

let multi f = function
    | x::(y::tl) as xs -> f xs
    | [x] -> x
    | [] -> f []

let delimSurround sep head tail xs = xs |> (delim sep >> surround head tail) 

let rec exprIsReturnable =
    function
    | ExprApp _
    | ExprInfixApp _
    | ExprConst _
    | ExprLambda _
    | ExprList _
    | ExprMatch _
    | ExprMatchLambda _
    | ExprRecord _
    | ExprTuple _
    | ExprVal _ -> true
    | ExprWithType (_,e) -> exprIsReturnable e
    | _ -> false

let rec getTyp =
    function
    | TypType (TypeId x) -> x
    | TypGeneric (GenericId x) -> x
    | TypWithGeneric(gs, x) -> (gs @ [x]) |> List.map getTyp |> delim " "
    | TypFun(t1, t2) -> [t1; t2] |> Seq.map getTyp |> delim " -> "
    | TypTuple(ts) -> ts |> Seq.map getTyp |> delim ", " |> surround "Tuple<" ">"

let rec getPat =
    function
    | PatConst (ConstId c) -> match c with | "()" -> "" | _ -> c
    | PatWildcard -> "_"
    | PatBind (ValId v) -> v
    | PatCons (ValId v, ps) -> v + ((ps |> Seq.map getPat |> delim ", ") |> surround "(" ")")
    | PatInfixCons (p1, (ValId v), p2) -> [getPat p1; v; getPat p2] |> delim " " |> surround "(" ")"
    | PatTuple ts -> ts |> List.map getPat |> delimSurround ", " "Tuple(" ")" 
    | PatList ts -> ts |> List.map getPat |> delimSurround "; " "[" "]" 
    | PatRecord rows -> rows |> Seq.map (fun (FieldId f, p) -> f + " : " + getPat p) |> delim ", " |> surround "{" "}" 
    | PatWithType (t, p) -> getPat p + " : " + getTyp t
    | PatBindAs (ValId v, p) -> getPat p + " as " + v

let rec getDecl =
    function
    | TypeDeclRecord rows -> rows |> Seq.map (fun (FieldId f, t) -> f + " : " + getDecl t) |> delim ", " |> surround "{" "}"
    | TypeDeclUnion rows -> rows |> Seq.map (fun (ValId v, t) -> v + (t |> Option.map (fun x -> getDecl x |> surround "(" ")") |> Option.fill "")) |> delimSurround " ; " "{" "}"
    | TypeDeclTuple ts -> ts |> Seq.map getDecl |> delim ", " |> surround "Tuple(" ")"
    | TypeDeclId (TypeId p) -> p
    //| TypeDeclWithGeneric (GenericId g, t) -> [g; getDecl t] |> delim " "

let rec flattenExprList f xs =
    xs |> List.collect (fun x -> match f x with | Some xs2 -> flattenExprList f xs2 | None -> [x])

let toSequence =
    function
    | ExprSequence _ as e -> e
    | x -> ExprSequence [x]

let rec getMatch (p, whenE, e) =
    let whenClause = whenE |> Option.map (fun x -> " if(" + getExpr x + ") ") |> Option.fill ""
    [getPat p + whenClause; getExpr (toSequence e)] |> delim " : "

and getBind isRec isFirstRec (p, e) =
    match p,e with
    | PatCons _, _ ->
        "static public function " + getPat p + getExpr (toSequence e)
    | _, ExprLambda (args, e) -> 
        "static public function " + getPat p + (args |> Seq.map getPat |> delimSurround ", " "(" ")") + getExpr (toSequence e)
    | _ -> "var " + getPat p + " = " + getExpr e

and getExpr =
    function
    | ExprConst (ConstId c) -> match c with | "()" -> "{}" | _ -> c
    | ExprVal (ValId v) -> v
    | ExprApp (e1, ExprConst (ConstId "()")) -> getExpr e1 + "()"
    | ExprApp (e1, e2) -> getExpr e1 + (getExpr e2 |> surround "(" ")")
    | ExprInfixApp (e1, ValId v, e2) -> [getExpr e1; v; getExpr e2] |> delim " "
    | ExprTuple ts -> ts |> List.map getExpr |> delimSurround ", " "Tuple(" ")"
    | ExprList ts -> ts |> List.map getExpr |> delimSurround ", " "[" "]"
    | ExprRecord (copyE, rows) -> 
        let fields = rows |> Seq.map (fun (FieldId f, e) -> f + " = " + getExpr e) |> delim "; " 
        let copyStat = copyE |> Option.map (fun x -> getExpr x + " with ") |> Option.fill ""
        copyStat + fields |> surround "{" "}"
    | ExprBind (_,p,e) -> 
        getBind false false (p,e)
    | ExprRecBind bindings -> 
        let n = Seq.length bindings
        (bindings |> Seq.mapi (fun i x -> getBind true (i=0) x) |> delim "") 
    | ExprMatch (e, rows) -> 
        (["switch"; getExpr e |> surround "(" ")"] |> delim " ")
        + ((rows |> Seq.map (fun m -> "case " + getMatch m + ";") |> delim "") |> surround "{" "}")
    | ExprLambda (args, e) -> "function " + (args |> Seq.map getPat |> delimSurround ", " "(" ")") + getExpr (toSequence e)
    | ExprWithType (t, e) -> getExpr e + " : " + getTyp t
    | ExprModule (ModuleId m, e) -> "class " + m + getExpr e
    | ExprType (TypeId tId, t) -> 
        let prefix =
            match t with
            | TypeDeclRecord _ -> "typedef " + tId + " = "
            | TypeDeclUnion rows -> "enum " + tId
            | TypeDeclTuple ts -> ts |> Seq.map getDecl |> delim ", " |> surround "Tuple<" ">"
            | TypeDeclId (TypeId p) -> p
            //| TypeDeclWithGeneric (GenericId g, t) -> getDecl t + (g |> surround "<" ">")
        prefix + getDecl t
    //| ExprNewType (TypeId tId, t) -> failwith "not supported"
    | ExprInclude (ModuleId m) -> "load " + m
    | ExprSequence [] -> ""
    | ExprSequence ((ExprModule _ as m) :: es) -> getExpr m + getExpr (ExprSequence es)
    | ExprSequence es -> 
        let n = Seq.length es
        es |> flattenExprList (function |ExprSequence es2 -> Some es2 |_ -> None)
        |> Seq.mapi (fun i e -> 
            if i = n-1 && exprIsReturnable e then "return " else ""
            + getExpr e + 
                match e with
                | ExprModule _ -> ""
                | _ -> ";")
        |> delim "" |> surround "{" "}"

let format (str: string) =
    let ind x = String.replicate (x*2) " "
    let rec f indent str =
        
        match str with
        | [] -> []
        | "{"::xs -> "{" + nl + ind (indent+1) :: f (indent+1) xs
        | "}"::";"::xs -> "};" + nl + ind (indent-1) :: f (indent-1) xs
        | ";"::"}"::xs -> ";" + nl + ind (indent-1) + "}" :: f (indent-1) xs
        | "}"::xs -> "}" + nl + ind (indent-1) :: f (indent-1) xs
        | ";"::xs -> ";" + nl + ind indent :: f indent xs
        | c::xs -> c :: f indent xs
    
    f 0 (str |> Seq.map string |> Seq.toList)
    |> String.concat ""

let getExprAndFormat e = e |> getExpr |> format
