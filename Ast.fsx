module MLASK

module AST =
    type ModuleId = ModuleId of string
    type TypeId = TypeId of string
    type GenericId = GenericId of string
    type PrimitiveId = PrimitiveId of string
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
    | PatCons of ValId * Pat
    | PatInfixCons of Pat * ValId * Pat
    | PatTuple of Pat list
    | PatRecord of (FieldId * Pat) list
    | PatWithType of Typ * Pat
    | PatBindAs of ValId * Pat
    
    type TypeDecl = 
    | TypeDeclRecord of (FieldId * TypeDecl) list
    | TypeDeclUnion of (ValId * TypeDecl option) list
    | TypeDeclTuple of TypeDecl list
    | TypeDeclPrimitive of PrimitiveId
    | TypeDeclWithGeneric of GenericId * TypeDecl
    
    type Expr =
    | ExprConst of ConstId
    | ExprVal of ValId
    | ExprApp of Expr * Expr //application
    | ExprInfixApp of Expr * ValId * Expr
    | ExprTuple of Expr list
    | ExprRecord of (FieldId * Expr) list
    | ExprSequence of Expr list // command1; commmand2; Expr
    | ExprRecSequence of Expr list // command1; commmand2; Expr
    | ExprBind of (Pat list) * Expr // let x = expr //TODO let rec
    | ExprMatch of Expr * Match list
    | ExprFun of Match
    | ExprWithType of Typ * Expr
    | ExprModule of ModuleId * Expr
    | ExprType of TypeId * TypeDecl
    | ExprNewType of TypeId * TypeDecl
    | ExprInclude of ModuleId

    and Match = Pat * Expr

    type Program = Program of Expr