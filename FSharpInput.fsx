#load "Include.fsx"

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open MLASK.AST

let checker = FSharpChecker.Create()

let getUntypedTree (file, input) = 
  // Get compiler options for the 'project' implied by a single script file
  let projOptions = 
      checker.GetProjectOptionsFromScript(file, input)
      |> Async.RunSynchronously

  // Run the first phase (untyped parsing) of the compiler
  let parseFileResults = 
      checker.ParseFileInProject(file, input, projOptions) 
      |> Async.RunSynchronously

  match parseFileResults.ParseTree with
  | Some tree -> tree
  | None -> failwith "Something went wrong during parsing!"

let getFsAst file =
    let input = System.IO.File.ReadAllText file
    getUntypedTree(file, input)

let toAST(file) =

    let constE x = ExprConst (ConstId x)
    let multi f = function
        | x::(y::tl) as xs -> f xs
        | [x] -> x
        | [] -> f []

    let tree = getFsAst file

    let visitLongIdent (ident: LongIdent) =
        let names = String.concat "." [ for i in ident -> i.idText ]
        sprintf "%s" names

    let visitConst = function
        | SynConst.String(lit,_) -> sprintf "\"%s\"" lit |> ConstId
        | SynConst.Bool(x) -> sprintf "%A" x |> ConstId
        | SynConst.Int32(x) -> sprintf "%i" x |> ConstId
        | SynConst.Unit -> ConstId "()"
        | x -> failwithf "[Const: %A]" x

    let rec visitConstrArgs = function
        | SynConstructorArgs.Pats pats -> pats |> List.map visitPattern |> PatTuple
        | x -> failwithf "[ConstrArgs: %A]" x

    and visitPattern = function
    | SynPat.Wild(_) -> PatWildcard
    | SynPat.Named(SynPat.Wild(_), name, _, _, _) -> PatBind(ValId name.idText)
    | SynPat.Named(pat, name, _, _, _) -> PatBindAs(ValId name.idText, visitPattern pat)
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, pats, _, _) ->
        PatCons(ValId (visitLongIdent ident), visitConstrArgs pats)
    | SynPat.Paren(expr,_) -> visitPattern expr
    | SynPat.Const(c,_) -> PatConst (visitConst c)
    | pat -> failwithf "[pattern: %A]" pat

    let rec getBind isRec bindings =
        bindings |> Seq.map (fun binding ->
            let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, 
                        data, pat, retInfo, init, m, sp)) = binding
            (visitPattern pat, visitExpression init))
        |> Seq.toList 
        |> (fun x -> if isRec then ExprRecBind x else let (p,e) = Seq.head x in ExprBind (p, e)) 
    
    and visitExpression = function
    | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
        let trueMatch = PatConst (ConstId "true"), None, visitExpression trueBranch
        let elseMatch = PatConst (ConstId "false"), None, defaultArg (falseBranchOpt |> Option.map visitExpression) (constE "()") 
        ExprMatch ((visitExpression cond), [trueMatch; elseMatch])

    | SynExpr.MatchLambda(_,_, matches,_,_) -> 
        matches |> List.map (function
            | Clause (pat, whenExpr, e, _, _) -> visitPattern pat, (whenExpr |> Option.map visitExpression), visitExpression e  
        ) |> ExprMatchLambda 

    | SynExpr.LetOrUse(isRec, _, bindings, body, _) ->
        // Visit bindings (there may be multiple 
        // for 'let .. = .. and .. = .. in ...'
        //printfn "LetOrUse with the following bindings:"
        ExprSequence [getBind isRec bindings; visitExpression body]
    
    | SynExpr.Const(c,_) -> visitConst c |> ExprConst
    | SynExpr.App(_,_,x,y,_) -> ExprApp(visitExpression x, visitExpression y)
    | SynExpr.Ident(ident) -> ExprVal (ValId ident.idText)
    | SynExpr.Paren(expr,_,_,_) -> visitExpression expr

    | SynExpr.Sequential(_, _, e1, e2, _) -> ExprSequence [visitExpression e1; visitExpression e2]

    | expr -> failwithf " - not supported expression: %A" expr

    let visitDeclarations decls = 
        decls |> Seq.map (fun declaration ->
            match declaration with
            | SynModuleDecl.Let(isRec, bindings, range) ->
                getBind isRec bindings
            | SynModuleDecl.DoExpr(_, e, _) -> visitExpression e
            | _ -> failwithf " - not supported declaration: %A" declaration)
        |> Seq.toList
        |> ExprSequence

    let visitModulesAndNamespaces modulesOrNss =
        modulesOrNss |> Seq.map (fun moduleOrNs ->
            let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = moduleOrNs
            ExprModule (ModuleId (visitLongIdent lid), visitDeclarations decls))
        |> Seq.toList |> ExprSequence

    match tree with
    | ParsedInput.ImplFile(implFile) ->
        let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
        visitModulesAndNamespaces modules
    | _ -> failwith "F# Interface file (*.fsi) not supported."
