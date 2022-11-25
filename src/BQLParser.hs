module BQLParser where

import Control.Monad (void)
import Data.Functor (($>))
import ParseLib (Parser)
import qualified ParseLib as P
import Control.Applicative
import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

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
  | ArrInd Exp Exp
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
  | Gt
  | Ge
  | Eq
  | NEq
  | Lt
  | Le 
  deriving (Show)

data Uop = 
    Neg
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

inBracketsP :: Parser a -> Parser a
inBracketsP p = P.between (P.string "[") p (P.string "]")

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
precLevel :: Bop -> Int
precLevel Mult = 12
precLevel Div = 12
precLevel Mod = 12
precLevel Add = 11
precLevel Sub = 11
precLevel Gt = 9
precLevel Ge = 9
precLevel Lt = 9
precLevel Le = 9
precLevel Eq = 8
precLevel NEq = 8
precLevel And = 4
precLevel Or = 3

opsAtLevel :: Int -> Parser (Exp -> Exp -> Exp)
opsAtLevel l = BOp <$> P.filter (\x -> precLevel x == l) bopP

bopP :: Parser Bop
bopP = trimP $
  P.string "*" $> Mult <|>
  P.string "/" $> Div <|>
  P.string "%" $> Mod <|>
  P.string "+" $> Add <|>
  P.string "-" $> Sub <|>
  P.string "<" $> Gt <|>
  P.string "<=" $> Ge <|>
  P.string ">" $> Lt <|>
  P.string ">=" $> Le <|>
  P.string "==" $> Eq <|>
  P.string "!=" $> NEq <|>
  P.string "and" $> And <|>
  P.string "or" $> Or 

uopP :: Parser Uop
uopP = trimP $
  P.string "-" $> Neg <|>
  P.string "!" $> Not 

expP :: Parser Exp
expP = orP where
  orP = andP `P.chainl1` opsAtLevel (precLevel Or)
  andP = equalityP `P.chainl1` opsAtLevel (precLevel And)
  equalityP = compP `P.chainl1` opsAtLevel (precLevel Eq)
  compP = sumP `P.chainl1` opsAtLevel (precLevel Lt)
  sumP = multP `P.chainl1` opsAtLevel (precLevel Add)
  multP = indP `P.chainl1` opsAtLevel (precLevel Mult)
  indP = arrIndP unaryP
  unaryP = baseP <|> UOp <$> uopP <*> unaryP  
  baseP = expFCallP <|> Var <$> varNameP <|> Val <$> valueP

arrIndP :: Parser Exp -> Parser Exp
arrIndP p = process <$> first <*> rest where
  process :: Exp -> [Exp] -> Exp
  process = foldl comb 

  comb :: Exp -> Exp -> Exp
  comb = ArrInd

  first :: Parser Exp
  first = p

  rest :: Parser [Exp]
  rest = many $ inBracketsP expP

expFCallP :: Parser Exp
expFCallP = FCall <$> 
  (varNameP <* P.string "(") <*> 
  P.sepBy expP (trimP (P.string ",")) <*
  P.string ")"

expValP :: Parser Exp
expValP = Val <$> valueP

expVarP :: Parser Exp
expVarP = Var <$> varNameP

----- Statement/Block Parsing -----
blockP :: Parser Block
blockP = Block <$> many statementP

statementP :: Parser Statement
statementP = (letP <|> assignP <|> returnP) <* trimP (P.string ";")

varNameP :: Parser String
varNameP = (:) <$> P.alpha <*> many (P.alpha <|> P.digit)

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

----- Query Parser -----
data TopLevelStatement = S Statement | F FDecl

queryP :: Parser Query
queryP = queryCons <$> many ((F <$> fdeclP) <|> (S <$> statementP)) where
  queryCons :: [TopLevelStatement] -> Query
  queryCons = foldr aux (Query [] (Block [])) 

  aux :: TopLevelStatement -> Query -> Query
  aux s (Query fdecls (Block main)) = case s of 
    F f -> Query (f:fdecls) (Block main)
    S s -> Query fdecls (Block (s : main)) 


----- Pretty Printing -----
class PP a where
  pp :: a -> Doc 

instance PP Bop where
  pp Mult = PP.char '-'
  pp Div = PP.char '/'
  pp Mod = PP.char '%'
  pp Add = PP.char '+'
  pp Sub = PP.char '-'
  pp Gt = PP.char '<'
  pp Ge = PP.text "<="
  pp Lt = PP.char '>'
  pp Le = PP.text ">="
  pp Eq = PP.text "=="
  pp NEq = PP.text "!="
  pp And = PP.text "and"
  pp Or = PP.text "or"

instance PP Uop where
  pp Not = PP.char '!'
  pp Neg = PP.char '-'

instance PP Bool where
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP Value where
  pp (IntVal i) = PP.int i
  pp (StringVal s) = PP.text s
  pp (BoolVal b) = pp b
  pp _ = undefined 

instance PP Exp where
  pp (Val v) = pp v
  pp (Var s) = PP.text s
  pp (ArrInd a i) = pp a <> PP.char '[' <> pp i <> PP.char ']'
  pp (BOp o e1 e2) = pp e1 <> PP.char ' ' <> pp o <> PP.char ' ' <> pp e2
  pp (UOp o e) = pp o <+> pp e
  pp (FCall _ _) = undefined


  -- Val Value 
  -- | Var String
  -- | ArrInd Exp Exp
  -- | BOp Bop Exp Exp
  -- | UOp Uop Exp
  -- | FCall String [Exp]