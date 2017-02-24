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
    module Typ =
        type Typ =
        | Type of TypeId
        | Generic of GenericId
        | WithGeneric of GenericId * Typ
        | Fun of Typ * Typ
        | Tuple of Typ []

    type Typ = Typ.Typ

    // Pattern
    module Pat =
        type Pat =
        | Const of ConstId
        | Wildcard
        | Bind of ValId //binding to identificator
        | Cons of ValId * Pat
        | InfixCons of Pat * ValId * Pat
        | Tuple of Pat []
        | Record of (FieldId * Pat) []
        | WithType of Typ * Pat
        | BindAs of ValId * Pat

    type Pat = Pat.Pat
    
    module TypeDecl =
        type TypeDecl = 
        | Record of (FieldId * TypeDecl) []
        | Union of (ValId * TypeDecl option) []
        | Tuple of TypeDecl []
        | Primitive of PrimitiveId
        | WithGeneric of GenericId * TypeDecl
    type TypeDecl = TypeDecl.TypeDecl
    
    module Expr =
        type Expr =
        | Const of ConstId
        | Val of ValId
        | App of Expr * Expr //application
        | InfixApp of Expr * ValId * Expr
        | Tuple of Expr []
        | Record of (FieldId * Expr) []
        | Sequence of Expr [] // command1; commmand2; Expr
        | RecSequence of Expr [] // command1; commmand2; Expr
        | Bind of (Pat []) * Expr // let x = expr //TODO let rec
        | Match of Pat * Match []
        | Fun of Match
        | WithType of Typ * Expr
        | Module of ModuleId * Expr
        | Type of TypeId * TypeDecl
        | NewType of TypeId * TypeDecl
        | Include of ModuleId

        and Match = Pat * Expr

    type Expr = Expr.Expr

    type Program = Program of Expr