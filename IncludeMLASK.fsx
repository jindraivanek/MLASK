#if MLASK
#else
#define "MLASK"
#load ".paket/load/main.group.fsx"
#load "Ast.fsx"
#load "FSharpInput.fsx"
#load "OcamlOutput.fsx"
#load "HaxeOutput.fsx"
#r "packages/System.Runtime/lib/net462/System.Runtime.dll"
#I "packages/FSharp.Compiler.Service/lib/net45"
#endif