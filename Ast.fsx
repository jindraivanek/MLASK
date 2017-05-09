module MLASK

module AST =
    type ModuleId = ModuleId of string
    type TypeId = TypeId of string
    type GenericId = GenericId of string
    type ConstId = ConstId of string
    type ValId = ValId of string
    type FieldId = FieldId of string
    
    // type abbrevation, type definition in Decl module
    type Typ =
    | TypType of TypeId
    | TypGeneric of GenericId
    | TypWithGeneric of GenericId * Typ
    | TypFun of Typ * Typ
    | TypTuple of Typ list

    // Pattern
    type Pat =
    | PatConst of ConstId
    | PatWildcard
    | PatBind of ValId //binding to identificator
    | PatCons of ValId * Pat list
    | PatInfixCons of Pat * ValId * Pat
    | PatTuple of Pat list
    | PatList of Pat list
    | PatRecord of (FieldId * Pat) list
    | PatWithType of Typ * Pat
    | PatBindAs of ValId * Pat
    
    type TypeDecl = 
    | TypeDeclRecord of (FieldId * TypeDecl) list
    | TypeDeclUnion of (ValId * TypeDecl option) list
    | TypeDeclTuple of TypeDecl list
    | TypeDeclId of TypeId
    | TypeDeclWithGeneric of GenericId * TypeDecl
    
    type Expr =
    | ExprConst of ConstId
    | ExprVal of ValId
    | ExprApp of Expr * Expr //application
    | ExprInfixApp of Expr * ValId * Expr
    | ExprTuple of Expr list
    | ExprList of Expr list
    | ExprRecord of Expr option * (FieldId * Expr) list // recordToCopy, fields
    | ExprSequence of Expr list // command1; commmand2; Expr
    | ExprBind of Pat * Expr // let x = expr1
    | ExprRecBind of (Pat * Expr) list
    | ExprMatch of Expr * Match list
    | ExprMatchLambda of Match list
    | ExprLambda of Pat list * Expr
    | ExprWithType of Typ * Expr
    | ExprModule of ModuleId * Expr
    | ExprType of TypeId * TypeDecl
    | ExprNewType of TypeId * TypeDecl
    | ExprInclude of ModuleId

    and Match = Pat * Expr option * Expr

    type Program = Program of Expr

module AstTransform =
    open AST

    let mutable tmpValId = 0
    let getTmpValId() = 
        tmpValId <- tmpValId + 1
        sprintf "__tmp_%i" tmpValId

    let rec isDefExpr =
        function
        | ExprInclude _
        | ExprBind _ 
        | ExprRecBind _ 
        | ExprModule _ 
        | ExprType _ 
        | ExprNewType _ -> true
        | ExprWithType (_,e) -> isDefExpr e
        | _ -> false

    let rec transformExpr f e =
        let replacement = f e
        let g e =
            match f e with
            | Some e2 -> transformExpr f e2
            | None -> transformExpr f e
        match e with
        | ExprApp(e1, e2) -> ExprApp(g e1, g e2)
        | ExprConst c -> ExprConst c
        | ExprInclude m -> ExprInclude m
        | ExprInfixApp(e1, v, e2) -> ExprInfixApp(g e1, v, g e2)
        | ExprTuple es -> es |> List.map g |> ExprTuple
        | ExprList es -> es |> List.map g |> ExprList
        | ExprRecord (me, fields) -> ((Option.map g me), (fields |> List.map (fun (f, e) -> f, g e))) |> ExprRecord
        | ExprSequence es -> es |> List.map g |> ExprSequence
        | ExprBind (p, e) -> (p, g e) |> ExprBind
        | ExprRecBind xs ->  xs |> List.map (fun (p, e) -> p, g e) |> ExprRecBind
        | ExprMatch (e, m) -> (g e, m) |> ExprMatch
        | ExprMatchLambda m -> ExprMatchLambda m
        | ExprLambda (p, e) -> ExprLambda (p, g e)
        | ExprWithType (t, e) -> ExprWithType (t, g e)
        | ExprModule (m, e) -> ExprModule (m, g e)
        | ExprType (t, d) -> ExprType (t, d)
        | ExprNewType (t, d) -> ExprNewType (t, d)
        | ExprVal v -> ExprVal v
        
    let expandMatchLambda =    
        function
        | ExprMatchLambda m -> 
            Some(
                let tmp = getTmpValId()
                ExprLambda ([PatBind (ValId tmp)], ExprMatch (ExprVal (ValId tmp), m)))
        | _ -> None
        |> transformExpr

    let topLevelExprToMainFunction =
        function
        | ExprModule (m, ExprSequence(es)) ->
            let (defExpr, nonDefExpr) = es |> List.partition isDefExpr
            let mainFn = ExprBind (PatCons ((ValId "main"), [(PatConst (ConstId "()"))]), ExprSequence nonDefExpr)
            ExprModule (m, ExprSequence(defExpr @ [mainFn]))
            |> Some
        | _ -> None
        |> transformExpr

module AstAnalyse =
    open AST
    let createBindsDict e =
        let rec f prefix e =
            match e with
            | ExprSequence es -> es |> List.collect (f prefix)
            | ExprModule (ModuleId mId, e) -> f (prefix + mId + ".") e
            | ExprBind (PatBind (ValId v), e) -> [ValId v, []]
            | ExprBind (PatCons ((ValId v), pats), e) -> [ValId v, pats]
            | _ -> []
        f "" e |> Map.ofList