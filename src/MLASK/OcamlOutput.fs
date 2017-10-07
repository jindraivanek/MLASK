module MLASK.Outputs.OCaml

open MLASK.AST

let nl = System.Environment.NewLine

let surround head tail body = head + body + tail

let delim sep xs = xs |> String.concat sep

let multi f = function
    | x::(y::tl) as xs -> f xs
    | [x] -> x
    | [] -> f []

let delimSurround sep head tail xs = xs |> (delim sep >> surround head tail) 

let rec getTyp =
    function
    | TypType (TypeId x) -> x
    | TypGeneric (GenericId x) -> x
    | TypWithGeneric(gs, x) -> (gs @ [x]) |> List.map getTyp |> delim " "
    | TypFun(t1, t2) -> [t1; t2] |> Seq.map getTyp |> delim " -> "
    | TypTuple(ts) -> ts |> Seq.map getTyp |> delim " * " |> surround "(" ")"

let rec getPat =
    function
    | PatConst (ConstId c) -> c
    | PatWildcard -> "_"
    | PatBind (ValId v) -> v
    | PatCons (ValId v, ps) -> v + " " + (ps |> Seq.map getPat |> delim " ")
    | PatInfixCons (p1, (ValId v), p2) -> [getPat p1; v; getPat p2] |> delim " " |> surround "(" ")"
    | PatTuple ts -> ts |> List.map getPat |> delimSurround ", " "(" ")" 
    | PatList ts -> ts |> List.map getPat |> delimSurround "; " "[" "]" 
    | PatRecord rows -> rows |> Seq.map (fun (FieldId f, p) -> f + " = " + getPat p) |> delim "; " |> surround "{" "}" 
    | PatWithType (t, p) -> getPat p + " : " + getTyp t
    | PatBindAs (ValId v, p) -> getPat p + " as " + v

let rec getDecl =
    function
    | TypeDeclRecord rows -> rows |> Seq.map (fun (FieldId f, t) -> f + " : " + getDecl t) |> delim "; " |> surround "{" "}"
    | TypeDeclUnion rows -> rows |> Seq.map (fun (ValId v, t) -> v + (t |> Option.map (fun x -> " of " + getDecl x) |> Option.fill "")) |> delim " | "
    | TypeDeclTuple ts -> ts |> Seq.map getDecl |> delim " * " |> surround "(" ")"
    | TypeDeclId (TypeId p) -> p
    //| TypeDeclWithGeneric (GenericId g, t) -> [g; getDecl t] |> delim " "

let rec getMatch (p, whenE, e) =
    let whenClause = whenE |> Option.map (fun x -> " when " + getExpr x) |> Option.fill ""
    [getPat p + whenClause; getExpr e] |> delim " -> "

and getBind isRec isFirstRec (p, e) =
    match isRec, isFirstRec with
    | true, true -> "let rec "
    | true, false -> "and "
    | _ -> "let " 
    + getPat p + " = " + nl + getExpr e

and getExpr =
    function
    | ExprConst (ConstId c) -> c
    | ExprVal (ValId v) -> v
    | ExprApp (e1, e2) -> [getExpr e1; getExpr e2] |> delim " " |> surround "(" ")"
    | ExprInfixApp (e1, ValId v, e2) -> [getExpr e1; v; getExpr e2] |> delim " " |> surround "(" ")"
    | ExprTuple ts -> ts |> List.map getExpr |> delimSurround ", " "(" ")"
    | ExprList ts -> ts |> List.map getExpr |> delimSurround "; " "[" "]"
    | ExprRecord (copyE, rows) -> 
        let fields = rows |> Seq.map (fun (FieldId f, e) -> f + " = " + getExpr e) |> delim "; " 
        let copyStat = copyE |> Option.map (fun x -> getExpr x + " with ") |> Option.fill ""
        copyStat + fields |> surround "{" "}"
    | ExprBind (_,p,e) -> 
        getBind false false (p,e)
    | ExprRecBind bindings -> 
        let n = Seq.length bindings
        (bindings |> Seq.mapi (fun i x -> getBind true (i=0) x) |> delim nl) 
    | ExprMatch (e, rows) -> 
        (["match"; getExpr e; "with"] |> delim " ")
        + nl + (rows |> Seq.map (fun m -> getMatch m) |> delim (nl + "| "))
    | ExprMatchLambda (rows) -> 
        "function"
        + nl + (rows |> Seq.map (fun m -> getMatch m) |> delim (nl + "| "))
    | ExprLambda (args, e) -> "fun " + (args |> Seq.map getPat |> delim " ") + " -> " + getExpr e |> surround "(" ")"
    | ExprWithType (t, e) -> getExpr e + " : " + getTyp t
    | ExprModule (ModuleId m, e) -> "module " + m + " = struct " + nl + getExpr e + " end"
    | ExprType (TypeId tId, t) -> "type " + tId + " = " + getDecl t
    //| ExprNewType (TypeId tId, t) -> "datatype " + tId + " = " + getDecl t
    | ExprInclude (ModuleId m) -> "load " + m
    | ExprSequence es -> 
        let n = Seq.length es
        es |> Seq.mapi (fun i e -> 
            getExpr e + 
            (if i < n-1 then 
                match e with 
                |ExprBind _ 
                |ExprRecBind _ -> " in " 
                |ExprType _ -> ";;"
                |_ -> "; " 
            else ""))
        |> delim nl //|> surround "(" ")"
    
