{------------------------------------------------------------------------------
    uPuppet: AST
------------------------------------------------------------------------------}

module UPuppet.AST ( AST, Value(..), deRefArray, deRefHash,
        Scope(..), Variable(..), BinOp(..),
	ValueExp(..), DeRefExp(..), fromValues, valueBool, valueString, valueInt, valueFloat,
        mergeParams, IfCont(..), Statements(..), ProgramEle(..), Program, OptParameterList ) where

import Data.List(lookup)

{------------------------------------------------------------------------------
    AST
------------------------------------------------------------------------------}

type AST = Program

data Value =   ValueInt    Integer
             | ValueBool   Bool
             | ValueString String
             | ValueFloat  Float
             | ValueArray  ArrayValue
             | ValueHash   HashValue
             | ValueRef    String String
             deriving (Show,Eq)

type ArrayValue = [Value]
type HashValue  = [(Value, Value)]

deRefArray :: ArrayValue -> Integer -> DeRefExp 
deRefArray xs d = Values (xs!! (fromInteger d))

deRefHash :: HashValue -> Value -> DeRefExp
deRefHash ((x,v):vs) d | ((x,v):vs) == [] = error "deRefHash"
                       | x == d           = Values v 
                       | otherwise        = deRefHash vs d

-- define the date type for scope
data Scope = STop | SNode | SClass String | SDef Scope  
             deriving (Show, Eq)
-- define variables with or without scope
data Variable = LocalVar String | ScopeVar Scope String 
                deriving (Show,Eq)
-- define all binary operators
data BinOp = AddOp | DivOp | MinOp | TimOp
           | AndOp | OrOp  
           | EqOp  | UneqOp | GrtOp  | LessOp | GeqOp |LeqOp 
             deriving (Show,Eq)

-- define expressions in muPuppet
data ValueExp =   BinOps      BinOp ValueExp ValueExp
                | Not         ValueExp
                | Selector    ValueExp [(ValueExp, ValueExp)]
                | Array       [ValueExp]
                | Hash        [(Value, ValueExp)]
                | DeRef       DeRefExp
                deriving (Show,Eq)
-- define the dereference expression component "DeRefExp"
data DeRefExp = Var Variable
              | Values Value
              | ResRef String ValueExp
              | DeRefItem DeRefExp ValueExp
              deriving (Show,Eq)

-- check whether a value is an integer nubmer
valueInt :: ValueExp -> Maybe Integer
valueInt (DeRef (Values (ValueInt x))) = (Just x) 
valueInt _ = Nothing

-- check whether a value is a float number
valueFloat :: ValueExp -> Maybe Float
valueFloat (DeRef (Values (ValueFloat x))) = (Just x)
valueFloat _ = Nothing

-- check whether a value is a boolean value
valueBool :: ValueExp -> Maybe Bool
valueBool (DeRef (Values (ValueBool x)))  = (Just x)
valueBool _ = Nothing

-- check whether a value is a string value
valueString :: ValueExp -> Maybe String
valueString (DeRef (Values (ValueString x))) = (Just x)
valueString _ = Nothing

-- convert a value expression to a value
fromValues :: ValueExp -> Value
fromValues (DeRef (Values v)) = v


type ParameterList = [(String, ValueExp)]
type OptParameterList = [(String, Maybe ValueExp)]

{- there should be a simpler way to do this -}
{-mergeParams :: ParameterList -> ParameterList -> ParameterList
mergeParams [] ps         = ps
mergeParams ((a,x):as) ps = mergeParams as (Map.toList (Map.insert a x (Map.fromList ps)))
-}

-- merge a list of parameters and a list of name and expression pairs
mergeParams ::  ParameterList -> OptParameterList -> ParameterList
mergeParams args [] = args
mergeParams args ((x,dflt):params) = (x, arg):mergeParams args params
  where arg = case (Data.List.lookup x args, dflt) of
                (Just e, _) ->  e
                (Nothing, Just e) -> e
                (Nothing, Nothing) -> error ("missing value for parameter " ++ x)

-- define the data type of statements of muPuppet
data Statements = Skip
                |If                     ValueExp Statements (Maybe IfCont)
                |Unless                 ValueExp Statements (Maybe IfCont)
                |Case                   ValueExp [(ValueExp, Statements)]
                |Assignment             String ValueExp
                |Resource               String ValueExp [(String,ValueExp)]
                |Include                String
                |StatementsList         [Statements]
                |ClassDecl              String [(String, ValueExp)]
                |ClassCont              ParameterList Statements
                |ResTypeDecl            String ValueExp [(String,ValueExp)]
                |ResTypeCont            String ParameterList Statements
                |ScopeStat              Scope Statements
                deriving (Show, Eq)
-- define the component "IfCont"
data IfCont = Elseif                 ValueExp Statements (Maybe IfCont)
            | Else                   Statements
                   deriving (Show, Eq)

-- define the data type of elements in a program of mPuppet
data ProgramEle = ProSkip 
             |ProStatement          Statements
             |Node                  String Statements
             |DefResType            String OptParameterList Statements
             |Class                 String OptParameterList (Maybe String) Statements
             deriving (Show)

-- define the type of a program of muPuppet
type Program = [ProgramEle] 
