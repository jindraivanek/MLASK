#load "Include.fsx"

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast


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


