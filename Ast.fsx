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
    | TypeDeclPrimitive of PrimitiveId
    | TypeDeclWithGeneric of GenericId * TypeDecl
    
    type Expr =
    | ExprConst of ConstId
    | ExprVal of ValId
    | ExprApp of Expr * Expr //application
    | ExprInfixApp of Expr * ValId * Expr
    | ExprTuple of Expr list
    | ExprList of Expr list
    | ExprRecord of (FieldId * Expr) list
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