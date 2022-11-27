module BQLParser where

import Data.Maybe as Maybe
import Control.Monad (void)
import Data.Functor (($>))
import ParseLib (Parser)
import qualified ParseLib as P
import Control.Applicative
import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP
import Data.List (intercalate)

----- Structure of BQL Query -----
data VarDecl = VDecl BType String deriving (Show)
data BType = 
    VoidT
  | BoolT
  | IntT
  | StringT
  | ArrayT BType
  | AnyT -- Only to be used internally with empty arrays, can't be used by users
  deriving (Show, Eq)

-- | The first argument contains all function declarations, the second is the main
-- code to be executed
data Query = Query [FDecl] Block deriving (Show)
data FDecl = FDecl String [VarDecl] Block deriving (Show)
newtype Block = Block [Statement] deriving (Show)
data Statement =
    Assign LValue Exp
  | Let VarDecl Exp
  | If Exp Block Block
  | Return Exp
  deriving (Show)

data Exp = 
    Val Value 
  | Var String
  | ArrInd Exp Exp
  | ArrCons [Exp]
  | BOp Bop Exp Exp
  | UOp Uop Exp
  | FCall String [Exp]
  deriving (Show)

data Value =
    BoolVal Bool
  | IntVal Int
  | StringVal String
  | ArrayVal [Value]
  deriving (Show)

data LValue =
    LVar String
  | LArrInd LValue Exp
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
  P.string "<=" $> Ge <|>
  P.string ">=" $> Le <|>
  P.string "<" $> Gt <|>
  P.string ">" $> Lt <|>
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
  baseP = expFCallP <|>
          expArrP <|>
          inParensP expP <|> 
          Val <$> valueP <|> 
          Var <$> varNameP 

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

expArrP :: Parser Exp
expArrP = ArrCons <$> inBracketsP (P.sepBy expP (trimP (P.string ",")))

lvalP :: Parser LValue
lvalP = process <$> first <*> rest where
  process :: LValue -> [Exp] -> LValue
  process = foldl comb 

  comb :: LValue -> Exp -> LValue
  comb = LArrInd

  first :: Parser LValue
  first = LVar <$> varNameP

  rest :: Parser [Exp]
  rest = many $ inBracketsP expP

----- Statement/Block Parsing -----
blockP :: Parser Block
blockP = Block <$> many statementP

statementP :: Parser Statement
statementP = letP <|> assignP <|> returnP <|> ifP

varNameP :: Parser String
varNameP = (:) <$> P.alpha <*> many (P.alpha <|> P.digit)

typedVarP :: Parser VarDecl
typedVarP = flip VDecl <$> varNameP <*> (trimP (P.string ":") *> typeP)

letP :: Parser Statement
letP = Let <$> (trimP (P.string "let ") *> typedVarP) <*> (trimP (P.string "=") *> expP) <* trimP (P.string ";")

assignP :: Parser Statement
assignP = Assign <$> trimP lvalP <*> (trimP (P.string "=") *> expP) <* trimP (P.string ";")

returnP :: Parser Statement 
returnP = Return <$> (trimP (P.string "return ") *> expP) <* trimP (P.string ";")

ifP :: Parser Statement
ifP = If <$>
  (P.string "if" *> inParensP expP) <*>
  trimP (inBracesP blockP) <*>
  ( trimP (P.string "else" *> trimP (inBracesP blockP)) <|> pure (Block []))

----- Function Declaration Parsing -----
fdeclP :: Parser FDecl
fdeclP = FDecl <$> 
  (trimP (P.string "func ") *> varNameP) <*>
  trimP (inParensP (P.sepBy typedVarP (trimP (P.string ",")))) <*>
  trimP (inBracesP blockP) 

----- Query Parser -----
data TopLevelStatement = S Statement | F FDecl

queryP :: Parser Query
queryP = queryCons <$> many ((F <$> fdeclP) <|> (S <$> statementP)) <* P.eof where
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

instance PP BType where
  pp VoidT = PP.text "void"
  pp BoolT = PP.text "bool"
  pp IntT = PP.text "int"
  pp StringT = PP.text "string"
  pp (ArrayT t) = PP.brackets $ pp t
  pp AnyT = PP.text "any"

instance PP Value where
  pp (IntVal i) = PP.int i
  pp (StringVal s) = PP.text s
  pp (BoolVal b) = pp b
  pp (ArrayVal l) = PP.brackets $ joinBy PP.comma (map pp l) where 
    joinBy :: Doc -> [Doc] -> Doc
    joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty

instance PP LValue where
  pp (LVar s) = PP.text s
  pp (LArrInd arr ind) = pp arr <> PP.brackets (pp ind)

instance PP Exp where
  pp (Val v) = pp v
  pp (Var s) = PP.text s
  pp (ArrInd a i) = pp a <> PP.char '[' <> pp i <> PP.char ']'
  pp (BOp o e1 e2) = pp e1 <> PP.char ' ' <> pp o <> PP.char ' ' <> pp e2
  pp (UOp o e) = pp o <+> pp e
  pp (FCall name args) = PP.text name <> PP.parens (joinBy PP.comma (map pp args)) where
    joinBy :: Doc -> [Doc] -> Doc
    joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty
  pp (ArrCons exps) = PP.brackets $ joinBy PP.comma (map pp exps) where 
    joinBy :: Doc -> [Doc] -> Doc
    joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty

instance PP VarDecl where
  pp (VDecl t n) = pp t <+> PP.char ':' <+> PP.text n

instance PP Statement where
  pp (Assign s e) = (pp s <+> PP.char '=' <+> pp e) <> PP.char ';'
  pp (Let v e) = (pp v <+> PP.char '=' <+> pp e) <> PP.char ';'
  pp (If e b1 (Block [])) = 
    PP.text "if" <> PP.parens (pp e) <> PP.text " {" PP.$$
    PP.nest 4 (pp b1) PP.$$ PP.text "}"
  pp (If e b1 b2) = 
    PP.text "if" <> PP.parens (pp e) <> PP.text " {" PP.$$
    PP.nest 4 (pp b1) PP.$$ PP.text "} else {" PP.$$
    PP.nest 4 (pp b2) PP.$$ PP.text "}"
  pp (Return e) = (PP.text "return" <+> pp e) <> PP.char ';'

instance PP FDecl where
  pp (FDecl name args b) = 
    PP.text "func " <> PP.text name <> PP.parens (joinBy PP.comma (map pp args)) <> PP.text " {" PP.$$
    PP.nest 4 (pp b) PP.$$
    PP.text "}" where
      joinBy :: Doc -> [Doc] -> Doc
      joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty

instance PP Block where
  pp (Block stmts) = PP.vcat (map pp stmts)

instance PP Query where
  pp (Query fdecls main) = PP.vcat (map pp fdecls) PP.$$ pp main