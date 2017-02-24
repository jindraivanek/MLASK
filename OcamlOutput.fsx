#load "Include.fsx"
#load "Ast.fsx"

open MLASK.AST

let nl = System.Environment.NewLine

let surround head tail body = head + body + tail

let delim sep xs = xs |> String.concat sep

let rec getTyp =
    function
    | Typ.Type (TypeId x) -> x
    | Typ.Generic (GenericId x) -> x
    | Typ.WithGeneric(GenericId g, x) -> [g; getTyp x] |> delim " "
    | Typ.Fun(t1, t2) -> [t1; t2] |> Seq.map getTyp |> delim " -> "
    | Typ.Tuple(ts) -> ts |> Seq.map getTyp |> delim " * " |> surround "(" ")"

let rec getPat =
    function
    | Pat.Const (ConstId c) -> c
    | Pat.Wildcard -> "_"
    | Pat.Bind (ValId v) -> v
    | Pat.Cons (ValId v, p) -> v + " " + getPat p |> surround "(" ")"
    | Pat.InfixCons (p1, (ValId v), p2) -> [getPat p1; v; getPat p2] |> delim " " |> surround "(" ")"
    | Pat.Tuple ts -> ts |> Seq.map getPat |> delim " * " |> surround "(" ")" 
    | Pat.Record rows -> rows |> Seq.map (fun (FieldId f, p) -> f + " = " + getPat p) |> delim "; " |> surround "{" "}" 
    | Pat.WithType (t, p) -> getPat p + " : " + getTyp t
    | Pat.BindAs (ValId v, p) -> getPat p + " as " + v

let rec getDecl =
    function
    | TypeDecl.Record rows -> rows |> Seq.map (fun (FieldId f, t) -> f + " : " + getDecl t) |> delim "; " |> surround "{" "}"
    | TypeDecl.Union rows -> rows |> Seq.map (fun (ValId v, t) -> v + (t |> Option.map (fun x -> " of " + getDecl x) |> Option.fill "")) |> delim " | "
    | TypeDecl.Tuple ts -> ts |> Seq.map getDecl |> delim " * " |> surround "(" ")"
    | TypeDecl.Primitive (PrimitiveId p) -> p
    | TypeDecl.WithGeneric (GenericId g, t) -> [g; getDecl t] |> delim " "

let rec getExpr =
    function
    | Expr.Const (ConstId c) -> c
    | Expr.Val (ValId v) -> v
    | Expr.App (e1, e2) -> [getExpr e1; getExpr e2] |> delim " " |> surround "(" ")"
    | Expr.InfixApp (e1, ValId v, e2) -> [getExpr e1; v; getExpr e2] |> delim " " |> surround "(" ")"
    | Expr.Tuple ts -> ts |> Seq.map getExpr |> delim ", " |> surround "(" ")"
    | Expr.Record rows -> rows |> Seq.map (fun (FieldId f, e) -> f + " = " + getExpr e) |> delim "; " |> surround "{" "}"
    | Expr.Bind (ps, e) -> "let " + (ps |> Seq.map getPat |> delim " ") + " = " + getExpr e + " in"
    | Expr.Match (p, rows) -> 
        (["match"; getPat p; "with"; "|"] |> delim " ")
        + " " + (rows |> Seq.map (fun (p, e) -> [getPat p; getExpr e] |> delim " -> ") |> delim " | ")
    | Expr.Fun (p, e) -> "fun " + getPat p + " -> " + getExpr e |> surround "(" ")"
    | Expr.WithType (t, e) -> getExpr e + " : " + getTyp t
    | Expr.Module (ModuleId m, e) -> "module " + m + " = struct " + getExpr e + " end"
    | Expr.Type (TypeId tId, t) -> "type " + tId + " = " + getDecl t
    | Expr.NewType (TypeId tId, t) -> "datatype " + tId + " = " + getDecl t
    | Expr.Include (ModuleId m) -> "load " + m
    | Expr.Sequence es -> 
        es |> Seq.map (fun e -> getExpr e + (match e with |Expr.Bind _ -> " " |_ -> "; "))
        |> delim "" |> surround "(" ")"
    | Expr.RecSequence es -> failwith "not supported"
