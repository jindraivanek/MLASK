module MLASK.Inputs.FSharp

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open MLASK.AST

let rec flatten f x = f x |> Seq.collect (fun x -> flatten f x)

let checker = FSharpChecker.Create()

let getUntypedTree (file, input) = 
  // Get compiler options for the 'project' implied by a single script file
  let (projOptions, _) = 
      checker.GetProjectOptionsFromScript(file, input)
      |> Async.RunSynchronously

  // Run the first phase (untyped parsing) of the compiler
  let parseFileResults, checkFileAnswer = 
      checker.ParseAndCheckFileInProject(file, 0, input, projOptions) 
      |> Async.RunSynchronously
  
  let checkFileResults = 
    match checkFileAnswer with
    | FSharpCheckFileAnswer.Succeeded(res) -> res
    | res -> failwithf "Parsing did not finish... (%A)" res

  match parseFileResults.ParseTree with
  | Some tree -> tree, checkFileResults
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

    let (tree, checkResult) = getFsAst file

    let visitLongIdent (ident: LongIdent) =
        let names = String.concat "." [ for i in ident -> i.idText ]
        names

    let visitConst = function
        | SynConst.String(lit,_) -> sprintf "\"%s\"" lit |> ConstId
        | SynConst.Bool(x) -> sprintf "%A" x |> ConstId
        | SynConst.Int32(x) -> sprintf "%i" x |> ConstId
        | SynConst.Double(x) -> sprintf "%f" x |> ConstId
        | SynConst.Unit -> ConstId "()"
        | x -> failwithf "[Const: %A]" x

    let rec visitConstrArgs = function
        | SynConstructorArgs.Pats pats -> pats |> List.map visitPattern
        | x -> failwithf "[ConstrArgs: %A]" x

    and getSimplePats = function
        | SynSimplePats.SimplePats (pats, _) -> pats |> List.map (function
            | SynSimplePat.Id(ident,_,_,_,_,_) -> PatBind(ValId ident.idText))

    and visitPattern = function
    | SynPat.Wild(_) -> PatWildcard
    | SynPat.Named(SynPat.Wild(_), name, _, _, _) -> PatBind(ValId name.idText)
    | SynPat.Named(pat, name, _, _, _) -> PatBindAs(ValId name.idText, visitPattern pat)
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, SynConstructorArgs.Pats [SynPat.Tuple([x;y],_)], _, _) 
        when String.startsWith "op_" (visitLongIdent ident) ->
            PatInfixCons(visitPattern x, ValId (visitLongIdent ident), visitPattern y)
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, pats, _, _) ->
        PatCons(ValId (visitLongIdent ident), visitConstrArgs pats)
    | SynPat.Paren(expr,_) -> visitPattern expr
    | SynPat.Const(c,_) -> PatConst (visitConst c)
    | SynPat.Tuple(xs,_) -> PatTuple (List.map visitPattern xs)
    | SynPat.ArrayOrList(_,xs,_) -> PatList (List.map visitPattern xs)
    | SynPat.Record(fields ,_) -> 
        fields |> List.map (fun ((_,name), pat) -> FieldId(name.idText), visitPattern pat)
        |> PatRecord
    | pat -> failwithf "[pattern: %A]" pat

    let rec getBind isRec bindings =
        bindings |> Seq.map (fun binding ->
            let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, 
                        data, pat, retInfo, init, m, sp)) = binding
            (visitPattern pat, visitExpression init))
        |> Seq.toList 
        |> (fun x -> if isRec then ExprRecBind x else let (p,e) = Seq.head x in ExprBind ([], p, e))

    and getMatches matches =
        matches |> List.map (function
            | Clause (pat, whenExpr, e, _, _) -> visitPattern pat, (whenExpr |> Option.map visitExpression), visitExpression e  
        )

    
    and visitExpression = function
    | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
        let trueMatch = PatConst (ConstId "true"), None, visitExpression trueBranch
        let elseMatch = PatConst (ConstId "false"), None, defaultArg (falseBranchOpt |> Option.map visitExpression) (constE "()") 
        ExprMatch ((visitExpression cond), [trueMatch; elseMatch])

    | SynExpr.Match(_,expr, matches,_,_) -> 
        ExprMatch (visitExpression expr, getMatches matches)
    
    | SynExpr.MatchLambda(_,_, matches,_,_) -> 
        getMatches matches |> ExprMatchLambda 

    | SynExpr.LetOrUse(isRec, _, bindings, body, _) ->
        // Visit bindings (there may be multiple 
        // for 'let .. = .. and .. = .. in ...'
        //printfn "LetOrUse with the following bindings:"
        ExprSequence [getBind isRec bindings; visitExpression body]

    | SynExpr.Lambda(_, _, args, body, _) -> ExprLambda (getSimplePats args, visitExpression body)
    
    | SynExpr.Const(c,_) -> visitConst c |> ExprConst
    | SynExpr.App(_,false, SynExpr.App(_,true, SynExpr.Ident ident,x,_),y,_) -> 
        ExprInfixApp(visitExpression x, ValId ident.idText, visitExpression y)
    | SynExpr.App(_,false,x,y,_) -> ExprApp(visitExpression x, visitExpression y)
    | SynExpr.App(_,true,SynExpr.Ident ident,SynExpr.Tuple([x;y], _, _),_) -> 
        let u = checkResult.GetSymbolUseAtLocation(ident.idRange.EndLine, ident.idRange.EndColumn, ident.idText, [])
        printfn "%A" u
        ExprInfixApp(visitExpression x, ValId ident.idText, visitExpression y)
    | SynExpr.Ident(ident) -> ExprVal (ValId ident.idText)
    | SynExpr.LongIdent(_, LongIdentWithDots(ident, _), _, _) ->
        ExprVal (ValId (visitLongIdent ident))
    | SynExpr.Paren(expr,_,_,_) -> visitExpression expr

    | SynExpr.Tuple(exprs, _, _) -> exprs |> List.map visitExpression |> ExprTuple
    | SynExpr.ArrayOrList(_, exprs,_) -> exprs |> List.map visitExpression |> ExprList
    | SynExpr.ArrayOrListOfSeqExpr(_,SynExpr.CompExpr(true,_,seqs,_),_) -> 
        seqs |> flatten (function |SynExpr.Sequential(_,_,e1,e2,_) -> [e1;e2] |_ -> [])
        |> Seq.map visitExpression |> Seq.toList |> ExprList
    | SynExpr.Record(_,copyInfo,fields,_) -> 
        let fs =
            fields |> List.choose (fun ((LongIdentWithDots(longId,_),_),e,_) -> 
                e |> Option.map (fun e -> FieldId(visitLongIdent longId), visitExpression e))
        let copyExpr = copyInfo |> Option.map (fst >> visitExpression)
        ExprRecord (copyExpr, fs) 

    | SynExpr.Sequential(_, _, e1, e2, _) -> ExprSequence [visitExpression e1; visitExpression e2]

    | expr -> failwithf " - not supported expression: %A" expr

    let getIdOfComponentInfo =
        function
        | ComponentInfo(_, _, _, longId, _, _, _, _) -> visitLongIdent longId |> TypeId
    
    let visitTypeDef =
        function
        | TypeDefn(compInfo, defnRepr, members, _) ->
            match defnRepr with
            | SynTypeDefnRepr.Simple(simpleRepr,_) ->
                match simpleRepr with
                | SynTypeDefnSimpleRepr.Record(_,fields,_) ->
                    let fs =
                        fields |> List.map (
                            function 
                            Field(_,_,id,typ,_,_,_,_) -> 
                                let fieldId = FieldId((id |> Option.get).idText) 
                                match typ with
                                | SynType.LongIdent(LongIdentWithDots(ids,_)) -> 
                                    fieldId, TypeDeclId(TypeId (visitLongIdent ids)))
                    Expr.ExprType(getIdOfComponentInfo compInfo, TypeDeclRecord fs) 
                        
    
    let visitDeclarations decls = 
        decls |> Seq.map (fun declaration ->
            match declaration with
            | SynModuleDecl.Let(isRec, bindings, range) ->
                getBind isRec bindings
            | SynModuleDecl.DoExpr(_, e, _) -> visitExpression e
            | SynModuleDecl.HashDirective _ -> constE "()"
            | SynModuleDecl.Types(typeDefs, _) -> 
                typeDefs |> List.map visitTypeDef |> multi ExprSequence 
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
