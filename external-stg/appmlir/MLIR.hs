-- | https://github.com/llvm/llvm-project/blob/master/mlir/docs/LangRef.md
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}
module MLIR where
import Prettyprinter

-- parenthesis: smooth sounds -> smooth curve -> ()
-- bracket: hard -ket sound -> rigid corner -> []
-- braces: shape looks like the braces people use for teeth -> {}

-- | TODO: how to get access to actual lines in SDoc so we can // all of the
-- lines?
newtype Comment = Comment (forall ann. Doc ann) -- any comment string
commentString :: String -> Comment
commentString s = Comment (pretty s)

instance Pretty (Comment) where
  pretty (Comment x) = pretty "//" <> x

-- // Identifiers
-- bare-id ::= (letter|[_]) (letter|digit|[_$.])*
newtype BareId = BareId String

instance Pretty BareId where
  pretty (BareId x) = pretty x

-- bare-id-list ::= bare-id (`,` bare-id)*
-- ssa-id ::= `%` suffix-id
-- suffix-id ::= (digit+ | ((letter|id-punct) (letter|id-punct|digit)*))
data SSAId = SSAId String

instance Pretty SSAId where
  pretty (SSAId x) = pretty ('%':x)

-- symbol-ref-id ::= `@` (suffix-id | string-literal)
data SymbolRefId = SymbolRefId String

instance Pretty SymbolRefId where
  pretty (SymbolRefId x) =  pretty "@" <> (dquotes (pretty x))
-- operation
-- region ::= `{` block* `}`
newtype Region = Region [Block]

defaultRegion :: Region
defaultRegion = Region []

instance Pretty Region where
  pretty (Region bs) = lbrace <+> nest 4 (vcat (map pretty bs)) <+> rbrace
 
-- generic-operation ::= string-literal `(` value-use-list? `)`  successor-list?
--                       (`(` region-list `)`)? attribute-dict? `:` function-type
-- region-list       ::= region (`,` region)*
newtype RegionList = RegionList [Region]
instance Pretty RegionList where
  pretty (RegionList []) = mempty
  pretty (RegionList rs) = parens (commaList (map pretty rs))

data Block = Block BlockLabel [Operation]

instance Pretty Block where
    pretty _ = error "unimplemented pretty for block"
  
  
-- block-label     ::= block-id block-arg-list? `:`
data BlockLabel = BlockLabel BlockId BlockArgList

instance Pretty BlockLabel where
  pretty (BlockLabel name args) = 
    let prettyArgs args = parens (commaList [pretty v <> colon <> pretty t | (v, t) <- args])
    in pretty name  <+> prettyArgs args <> colon
-- // Non-empty list of names and types.
-- value-id-and-type-list ::= value-id-and-type (`,` value-id-and-type)*
-- block-arg-list ::= `(` value-id-and-type-list? `)`
-- value-id-and-type ::= value-id `:` type
-- // Non-empty list of names and types.
-- value-id-and-type-list ::= value-id-and-type (`,` value-id-and-type)*
type BlockArgList = [(SSAId, Type)]-- [(ValueId, Type)]
-- block-id        ::= caret-id
-- caret-id        ::= `^` suffix-id
newtype BlockId = BlockId String -- BlockId SuffixId 
instance Pretty BlockId where
  pretty (BlockId x) = pretty ('^':x)
-- suffix-id ::= (digit+ | ((letter|id-punct) (letter|id-punct|digit)*))
newtype SuffixId = SuffixId String


-- attribute-dict ::= `{` `}`
--                  | `{` attribute-entry (`,` attribute-entry)* `}`
-- attribute-entry ::= dialect-attribute-entry | dependent-attribute-entry
-- dialect-attribute-entry ::= dialect-namespace `.` bare-id `=` attribute-value
-- dependent-attribute-entry ::= dependent-attribute-name `=` attribute-value
-- dependent-attribute-name ::= ((letter|[_]) (letter|digit|[_$])*)
--                            | string-literal
type AttributeName = String
data AttributeDict = AttributeDict [(AttributeName, AttributeValue)] 
  deriving(Monoid, Semigroup)

-- generic-operation ::= string-literal `(` value-use-list? `)`  successor-list?
--                       (`(` region-list `)`)? attribute-dict? `:` function-type
instance Pretty AttributeDict where
  pretty (AttributeDict []) = mempty
  pretty (AttributeDict nvs) = 
    braces . commaList $ [pretty name <+> equals <+> pretty val | (name, val) <- nvs]


-- attribute-value ::= attribute-alias | dialect-attribute | standard-attribute
-- standard-attribute ::=   affine-map-attribute
--                        | array-attribute
--                        | bool-attribute
--                        | dictionary-attribute
--                        | elements-attribute
--                        | float-attribute
--                        | integer-attribute
--                        | integer-set-attribute
--                        | string-attribute
--                        | symbol-ref-attribute
--                        | type-attribute
--                        | unit-attribute
-- 
data AttributeValue = AttributeSymbolRef SymbolRefId | 
                      AttributeString String | 
                      AttributeInteger Integer | 
                      AttributeType Type 


instance Pretty AttributeValue where
  pretty (AttributeSymbolRef x) = pretty x
  pretty (AttributeString x) = dquotes (pretty x)
  pretty (AttributeInteger x) = pretty x
  pretty (AttributeType x) = pretty x

-- operation         ::= op-result-list? (generic-operation | custom-operation)
--                       trailing-location?
-- generic-operation ::= string-literal `(` value-use-list? `)`  successor-list?
--                       (`(` region-list `)`)? attribute-dict? `:` function-type
data Operation = 
  Operation { opname :: String, 
              opvals :: ValueUseList, 
              opsuccs :: SuccessorList, 
              opregions :: RegionList,
              opattrs :: AttributeDict,
              opty :: FunctionType
            }

instance Pretty Operation where
  pretty op = 
       dquotes (pretty (opname op)) <> 
       parens (pretty (opvals op)) <>
       pretty (opsuccs op) <>
       (pretty (opregions op)) <>
       pretty (opattrs op) <> colon <> pretty (opty op)


-- | default operation.
defaultop :: Operation
defaultop = Operation "DEFAULTOP" (ValueUseList []) SuccessorList (RegionList [])  (AttributeDict []) defaultFunctionType


commaList :: [Doc ann] -> Doc ann
commaList xs = hcat $ punctuate comma xs


-- // MLIR functions can return multiple values.
-- function-result-type ::= type-list-parens
--                        | non-function-type
-- 
-- function-type ::= type-list-parens `->` function-result-type
data FunctionType = 
  FunctionType { 
    functionTypeParams :: [Type],
    functionTypeRets :: [Type]
  }

instance Pretty FunctionType where
  pretty (FunctionType ps rs) = 
    parens (commaList (map pretty ps)) <> pretty " -> " <> parens (commaList (map pretty rs))

-- | default function type
defaultFunctionType :: FunctionType; defaultFunctionType = FunctionType [] []

-- type ::= type-alias | dialect-type | standard-type
-- standard-type ::=     complex-type
--                     | float-type
--                     | function-type
--                     | index-type
--                     | integer-type
--                     | memref-type
--                     | none-type
--                     | tensor-type
--                     | tuple-type
--                     | vector-type
-- dialect-type ::= '!' opaque-dialect-item
-- opaque-dialect-item ::= dialect-namespace '<' string-literal '>'
-- signed-integer-type ::= `si` [1-9][0-9]*
-- unsigned-integer-type ::= `ui` [1-9][0-9]*
-- signless-integer-type ::= `i` [1-9][0-9]*
-- integer-type ::= signed-integer-type |
--                  unsigned-integer-type |
--                  signless-integer-type
-- 
data Type = TypeCustom String
    | TypeIntegerSignless Int  -- ^ width
instance Pretty Type where
  pretty (TypeCustom s) = pretty s
  pretty (TypeIntegerSignless i) = pretty 'i' <> pretty i

-- | successor-list    ::= successor (`,` successor)*
data SuccessorList = SuccessorList
instance Pretty SuccessorList where
  pretty (SuccessorList) = mempty
-- custom-operation  ::= bare-id custom-operation-format
-- op-result-list    ::= op-result (`,` op-result)* `=`
newtype OpResultList = NonEmpty OpResult
-- op-result         ::= value-id (`:` integer-literal)
newtype OpResult = OpResult String -- TODO: add the maybe int to pick certain results out
-- successor-list    ::= successor (`,` successor)*
-- successor         ::= caret-id (`:` bb-arg-list)?
-- region-list       ::= region (`,` region)*
-- trailing-location ::= (`loc` `(` location `)`)?
-- // Uses of value, e.g. in an operand list to an operation.



-- generic-operation ::= string-literal `(` value-use-list? `)`  successor-list?
--                       (`(` region-list `)`)? attribute-dict? `:` function-type
-- value-use ::= value-id
-- value-use-list ::= value-use (`,` value-use)*
newtype ValueUseList = ValueUseList [SSAId] -- [ValueId]
instance Pretty ValueUseList where
  pretty (ValueUseList []) = mempty
  pretty (ValueUseList [v]) = pretty v
  pretty (ValueUseList vs) = parens (commaList (map pretty vs))


-- value-id ::= `%` suffix-id
-- newtype ValueId = ValueId String


-- // This is a common way to refer to a value with a specified type.
-- ssa-use-and-type ::= ssa-use `:` type
-- 
-- // Non-empty list of names and types.
-- ssa-use-and-type-list ::= ssa-use-and-type (`,` ssa-use-and-type)*

-- dialect-namespace ::= bare-id
newtype DialectNamespace = DialectNamespace String
instance Pretty DialectNamespace where 
  pretty (DialectNamespace x) = pretty x
-- opaque-dialect-item ::= dialect-namespace '<' string-literal '>'
-- pretty-dialect-item ::= dialect-namespace '.' pretty-dialect-item-lead-ident
--                                               pretty-dialect-item-body?
-- pretty-dialect-item-lead-ident ::= '[A-Za-z][A-Za-z0-9._]*'
-- pretty-dialect-item-body ::= '<' pretty-dialect-item-contents+ '>'
-- pretty-dialect-item-contents ::= pretty-dialect-item-body
--                               | '(' pretty-dialect-item-contents+ ')'
--                               | '[' pretty-dialect-item-contents+ ']'
--                               | '{' pretty-dialect-item-contents+ '}'
--                               | '[^[<({>\])}\0]+'
-- 
-- dialect-type ::= '!' opaque-dialect-item
-- dialect-type ::= '!' pretty-dialect-item
data DialectType = DialectType  DialectNamespace String

-- Doc :: Doc -> String
data MLIRModule = MLIRModule {
  ops :: [Operation]
}

instance Pretty MLIRModule where
  pretty (MLIRModule ops) = pretty "module {" <+> vcat (map pretty ops) <+> pretty "}"
