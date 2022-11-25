module BQLParser
    ( queryP
    ) where

import Control.Monad (void)
import Data.Functor (($>))
import ParseLib (Parser)
import qualified ParseLib as P
import Control.Applicative

----- Structure of BQL Query -----
data Var = VDecl BType String deriving (Show)
data BType = 
    IntT
  | StringT
  | BoolT
  | VoidT
  | ArrayT BType
  deriving (Show)

-- | The first argument contains all function declarations, the second is the main
-- code to be executed
data Query = Query [FDecl] Block deriving (Show)
data FDecl = FDecl String [Var] Block deriving (Show)
newtype Block = Block [Statement] deriving (Show)
data Statement =
    Assign String Exp
  | Let Var Exp
  | If Exp Block Block
  | Return Exp
  deriving (Show)

data Exp = 
    Val Value 
  | Var String 
  | BOp Bop Exp Exp
  | UOp Uop Exp
  | FCall String [Exp]
  deriving (Show)

data Value =
    IntVal Int
  | StringVal String
  | BoolVal Bool
  | ArrayVal [Value]
  deriving (Show)
      
data Bop = 
    Add 
  | Sub
  | Mult
  | Div
  | Mod
  | And
  | Or
  | Xor
  deriving (Show)

data Uop = 
    Neg
  | Len
  | Not
  deriving (Show)

----- Auxiliary Parsing Types -----

----- Parsing Helpers -----
constP :: String -> a -> Parser a
constP s a = a <$ wsP (P.string s)

stringP :: String -> Parser ()
stringP s = void $ wsP $ P.string s

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

trimP :: Parser a -> Parser a
trimP p = many P.space *> p <* many P.space

inParensP :: Parser a -> Parser a
inParensP p = P.between (P.string "(") p (P.string ")")

inBracesP :: Parser a -> Parser a
inBracesP p = P.between (P.string "{") p (P.string "}")

----- Value Parsing -----
valueP :: Parser Value
valueP = intValP <|> stringValP <|> boolValP <|> arrayValP

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

stringValP :: Parser Value
stringValP = StringVal <$> P.between (P.string "\"") (many $ P.satisfy (/= '\"')) (stringP "\"")

boolValP :: Parser Value
boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

arrayValP :: Parser Value
arrayValP = ArrayVal <$> P.between (P.string "[") (P.sepBy valueP (wsP $ P.string ",")) (P.string "]")

----- Type Parsing -----
typeP :: Parser BType
typeP = intTypeP <|> stringTypeP <|> boolTypeP <|> arrayTypeP

intTypeP :: Parser BType
intTypeP = wsP (P.string "int") $> IntT

stringTypeP :: Parser BType 
stringTypeP = wsP (P.string "string") $> StringT

boolTypeP :: Parser BType 
boolTypeP = wsP (P.string "bool") $> BoolT

arrayTypeP :: Parser BType
arrayTypeP = ArrayT <$> P.between (P.string "[") typeP (P.string "]") 

----- Expression Parsing -----
expP :: Parser Exp
expP = expFcallP <|> expValP <|> expVarP

expFcallP :: Parser Exp
expFcallP = FCall <$> 
  (varNameP <* P.string "(") <*> 
  P.sepBy expP (trimP (P.string ",")) <*
  P.string ")"

expValP :: Parser Exp
expValP = Val <$> valueP

expVarP :: Parser Exp
expVarP = Var <$> varNameP

-- TODO: Add other expression types

----- Statement/Block Parsing -----
blockP :: Parser Block
blockP = Block <$> many statementP

statementP :: Parser Statement
statementP = (letP <|> assignP <|> returnP) <* trimP (P.string ";")

-- TODO: Make this correct
varNameP :: Parser String
varNameP = many (P.alpha <|> P.digit)

typedVarP :: Parser Var
typedVarP = flip VDecl <$> varNameP <*> (trimP (P.string ":") *> typeP)

letP :: Parser Statement
letP = Let <$> (trimP (P.string "let ") *> typedVarP) <*> (trimP (P.string "=") *> expP) 

assignP :: Parser Statement
assignP = Assign <$> varNameP <*> (trimP (P.string "=") *> expP) 

returnP :: Parser Statement 
returnP = Return <$> (trimP (P.string "return ") *> expP)

----- Function Declaration Parsing -----
fdeclP :: Parser FDecl
fdeclP = FDecl <$> 
  (trimP (P.string "func ") *> varNameP) <*>
  trimP (inParensP (P.sepBy typedVarP (trimP (P.string ",")))) <*>
  trimP (inBracesP blockP) 

-- Query Parser
data TopLevelStatement = S Statement | F FDecl

queryP :: Parser Query
queryP = queryCons <$> many ((F <$> fdeclP) <|> (S <$> statementP)) where
  queryCons :: [TopLevelStatement] -> Query
  queryCons = foldr aux (Query [] (Block [])) 

  aux :: TopLevelStatement -> Query -> Query
  aux s (Query fdecls (Block main)) = case s of 
    F f -> Query (f:fdecls) (Block main)
    S s -> Query fdecls (Block (s : main)) 