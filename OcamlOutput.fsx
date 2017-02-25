#load "Include.fsx"

open MLASK.AST

let nl = System.Environment.NewLine

let surround head tail body = head + body + tail

let delim sep xs = xs |> String.concat sep

let multi f = function
    | x::(y::tl) as xs -> f xs
    | [x] -> x
    | [] -> f []

let delimSurround sep head tail xs = xs |> multi (delim sep >> surround head tail) 

let rec getTyp =
    function
    | TypType (TypeId x) -> x
    | TypGeneric (GenericId x) -> x
    | TypWithGeneric(GenericId g, x) -> [g; getTyp x] |> delim " "
    | TypFun(t1, t2) -> [t1; t2] |> Seq.map getTyp |> delim " -> "
    | TypTuple(ts) -> ts |> Seq.map getTyp |> delim " * " |> surround "(" ")"

let rec getPat =
    function
    | PatConst (ConstId c) -> c
    | PatWildcard -> "_"
    | PatBind (ValId v) -> v
    | PatCons (ValId v, p) -> v + " " + getPat p
    | PatInfixCons (p1, (ValId v), p2) -> [getPat p1; v; getPat p2] |> delim " " |> surround "(" ")"
    | PatTuple ts -> ts |> List.map getPat |> delimSurround ", " "(" ")" 
    | PatRecord rows -> rows |> Seq.map (fun (FieldId f, p) -> f + " = " + getPat p) |> delim "; " |> surround "{" "}" 
    | PatWithType (t, p) -> getPat p + " : " + getTyp t
    | PatBindAs (ValId v, p) -> getPat p + " as " + v

let rec getDecl =
    function
    | TypeDeclRecord rows -> rows |> Seq.map (fun (FieldId f, t) -> f + " : " + getDecl t) |> delim "; " |> surround "{" "}"
    | TypeDeclUnion rows -> rows |> Seq.map (fun (ValId v, t) -> v + (t |> Option.map (fun x -> " of " + getDecl x) |> Option.fill "")) |> delim " | "
    | TypeDeclTuple ts -> ts |> Seq.map getDecl |> delim " * " |> surround "(" ")"
    | TypeDeclPrimitive (PrimitiveId p) -> p
    | TypeDeclWithGeneric (GenericId g, t) -> [g; getDecl t] |> delim " "

let rec getExpr =
    function
    | ExprConst (ConstId c) -> c
    | ExprVal (ValId v) -> v
    | ExprApp (e1, e2) -> [getExpr e1; getExpr e2] |> delim " " |> surround "(" ")"
    | ExprInfixApp (e1, ValId v, e2) -> [getExpr e1; v; getExpr e2] |> delim " " |> surround "(" ")"
    | ExprTuple ts -> ts |> List.map getExpr |> delimSurround ", " "(" ")"
    | ExprRecord rows -> rows |> Seq.map (fun (FieldId f, e) -> f + " = " + getExpr e) |> delim "; " |> surround "{" "}"
    | ExprBind (ps, e) -> "let " + (ps |> Seq.map getPat |> delim " ") + " = " + nl + getExpr e
    | ExprMatch (e, rows) -> 
        (["match"; getExpr e; "with"] |> delim " ")
        + nl + (rows |> Seq.map (fun (p, e) -> [getPat p; getExpr e] |> delim " -> ") |> delim (nl + "| "))
    | ExprFun (p, e) -> "fun " + getPat p + " -> " + getExpr e |> surround "(" ")"
    | ExprWithType (t, e) -> getExpr e + " : " + getTyp t
    | ExprModule (ModuleId m, e) -> "module " + m + " = struct " + nl + getExpr e + " end"
    | ExprType (TypeId tId, t) -> "type " + tId + " = " + getDecl t
    | ExprNewType (TypeId tId, t) -> "datatype " + tId + " = " + getDecl t
    | ExprInclude (ModuleId m) -> "load " + m
    | ExprSequence es -> 
        let n = Seq.length es
        es |> Seq.mapi (fun i e -> getExpr e + (if i < n-1 then match e with |ExprBind _ -> " in " |_ -> "; " else ""))
        |> delim nl //|> surround "(" ")"
    | ExprRecSequence es -> failwith "not supported"
