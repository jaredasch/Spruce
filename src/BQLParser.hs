module BQLParser where

import BQLTypes
import Control.Applicative
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe as Maybe
import ParseLib (Parser)
import ParseLib qualified as P
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

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
valueP = funcValP <|> intValP <|> stringValP <|> boolValP <|> arrayValP

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

stringValP :: Parser Value
stringValP = StringVal <$> P.between (P.string "\"") (many $ P.satisfy (/= '\"')) (stringP "\"")

boolValP :: Parser Value
boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

arrayValP :: Parser Value
arrayValP = ArrayVal <$> P.between (P.string "[") (P.sepBy valueP (wsP $ P.string ",")) (P.string "]")

funcValP :: Parser Value
funcValP =
  FunctionVal
    <$> (trimP (P.string "func") *> trimP (inParensP (P.sepBy typedVarP (trimP (P.string ",")))))
    <*> (trimP (P.string "->") *> typeP)
    <*> trimP (inBracesP blockP)

----- Type Parsing -----
typeP :: Parser BType
typeP = intTypeP <|> stringTypeP <|> boolTypeP <|> arrayTypeP <|> voidTypeP <|> funcTypeP

intTypeP :: Parser BType
intTypeP = wsP (P.string "int") $> IntT

stringTypeP :: Parser BType
stringTypeP = wsP (P.string "string") $> StringT

boolTypeP :: Parser BType
boolTypeP = wsP (P.string "bool") $> BoolT

voidTypeP :: Parser BType
voidTypeP = wsP (P.string "void") $> VoidT

funcTypeP :: Parser BType
funcTypeP = wsP (P.string "function") $> FuncAny

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
bopP =
  trimP $
    P.string "*" $> Mult
      <|> P.string "/" $> Div
      <|> P.string "%" $> Mod
      <|> P.string "+" $> Add
      <|> P.string "-" $> Sub
      <|> P.string "<=" $> Ge
      <|> P.string ">=" $> Le
      <|> P.string "<" $> Gt
      <|> P.string ">" $> Lt
      <|> P.string "==" $> Eq
      <|> P.string "!=" $> NEq
      <|> P.string "and" $> And
      <|> P.string "or" $> Or

uopP :: Parser Uop
uopP =
  trimP $
    P.string "-" $> Neg
      <|> P.string "!" $> Not

expP :: Parser Exp
expP = orP
  where
    orP = andP `P.chainl1` opsAtLevel (precLevel Or)
    andP = equalityP `P.chainl1` opsAtLevel (precLevel And)
    equalityP = compP `P.chainl1` opsAtLevel (precLevel Eq)
    compP = sumP `P.chainl1` opsAtLevel (precLevel Lt)
    sumP = multP `P.chainl1` opsAtLevel (precLevel Add)
    multP = indP `P.chainl1` opsAtLevel (precLevel Mult)
    indP = arrIndP unaryP
    unaryP = baseP <|> UOp <$> uopP <*> unaryP
    baseP =
      expFCallP
        <|> expArrP
        <|> inParensP expP
        <|> Val <$> valueP
        <|> Var <$> varNameP

arrIndP :: Parser Exp -> Parser Exp
arrIndP p = process <$> first <*> rest
  where
    process :: Exp -> [Exp] -> Exp
    process = foldl comb

    comb :: Exp -> Exp -> Exp
    comb = ArrInd

    first :: Parser Exp
    first = p

    rest :: Parser [Exp]
    rest = many $ inBracketsP expP

expFCallP :: Parser Exp
expFCallP =
  FCall
    <$> (varNameP <* P.string "(")
    <*> P.sepBy expP (trimP (P.string ","))
    <* P.string ")"

expValP :: Parser Exp
expValP = Val <$> valueP

expVarP :: Parser Exp
expVarP = Var <$> varNameP

expArrP :: Parser Exp
expArrP = ArrCons <$> inBracketsP (P.sepBy expP (trimP (P.string ",")))

lvalP :: Parser LValue
lvalP = process <$> first <*> rest
  where
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
blockP = Block <$> many (trimP statementP)

statementP :: Parser Statement
statementP = letP <|> assignP <|> returnP <|> ifP <|> fCallStatementP <|> whileP <|> forInP <|> atomicP

varNameP :: Parser String
varNameP = (:) <$> P.alpha <*> many (P.alpha <|> P.digit)

sharedP :: Parser Bool
sharedP = (P.string "shared " $> True) <|> (trimP (P.string "") $> False)

typedVarP :: Parser VarDecl
typedVarP = flip VDecl <$> sharedP <*> varNameP <*> (trimP (P.string ":") *> typeP)

letP :: Parser Statement
letP = Let <$> (trimP (P.string "let ") *> typedVarP) <*> (trimP (P.string "=") *> expP) <* trimP (P.string ";")

assignP :: Parser Statement
assignP = Assign <$> trimP lvalP <*> (trimP (P.string "=") *> expP) <* trimP (P.string ";")

returnP :: Parser Statement
returnP = Return <$> (trimP (P.string "return ") *> expP) <* trimP (P.string ";")

whileP :: Parser Statement
whileP =
  While
    <$> (trimP (P.string "while") *> trimP (inParensP expP))
    <*> inBracesP blockP

atomicP :: Parser Statement
atomicP = Atomic <$> (trimP (P.string "atomic") *> trimP (inBracesP blockP))

forInP :: Parser Statement
forInP =
  ForIn
    <$> (trimP (P.string "for") *> trimP (P.string "(") *> trimP typedVarP <* trimP (P.string "in "))
    <*> (expP <* trimP (P.string ")"))
    <*> trimP (inBracesP blockP)

ifP :: Parser Statement
ifP =
  If
    <$> (trimP (P.string "if") *> inParensP expP)
    <*> trimP (inBracesP blockP)
    <*> (trimP (P.string "else" *> trimP (inBracesP blockP)) <|> pure (Block []))

fCallStatementP :: Parser Statement
fCallStatementP =
  FCallStatement
    <$> (varNameP <* P.string "(")
    <*> P.sepBy expP (trimP (P.string ","))
    <* P.string ")"
    <* trimP (P.string ";")

----- Function Declaration Parsing -----
fdeclP :: Parser FDecl
fdeclP =
  FDecl
    <$> (trimP (P.string "func ") *> varNameP)
    <*> trimP (inParensP (P.sepBy typedVarP (trimP (P.string ","))))
    <*> trimP (trimP (P.string "->") *> typeP)
    <*> trimP (inBracesP blockP)

----- Program Parser -----
data TopLevelStatement = S Statement | F FDecl

programP :: Parser Program
programP = programCons <$> many ((F <$> fdeclP) <|> (S <$> statementP)) <* P.eof
  where
    programCons :: [TopLevelStatement] -> Program
    programCons = foldr aux (Program [] (Block []))

    aux :: TopLevelStatement -> Program -> Program
    aux s (Program fdecls (Block main)) = case s of
      F f -> Program (f : fdecls) (Block main)
      S s -> Program fdecls (Block (s : main))

----- Pretty Printing -----
class PP a where
  pp :: a -> Doc

instance PP Bop where
  pp Mult = PP.char '*'
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
  pp FuncAny = PP.text "func"

instance PP Value where
  pp (IntVal i) = PP.int i
  pp (StringVal s) = PP.char '"' <> (PP.text s) <> PP.char '"'
  pp (BoolVal b) = pp b
  pp (ArrayVal l) = PP.brackets $ joinBy PP.comma (map pp l)
    where
      joinBy :: Doc -> [Doc] -> Doc
      joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty

instance PP LValue where
  pp (LVar s) = PP.text s
  pp (LArrInd arr ind) = pp arr <> PP.brackets (pp ind)

instance PP Exp where
  pp (Val v) = pp v
  pp (Var s) = PP.text s
  pp (ArrInd a i) = pp a <> PP.char '[' <> pp i <> PP.char ']'
  pp e@(BOp {}) = ppPrec 0 e
    where
      ppPrec n (BOp op e1 e2) =
        ppParens (precLevel op < n) $
          ppPrec (precLevel op) e1 <+> pp op <+> ppPrec (precLevel op + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id
  pp (UOp o e) = pp o <+> PP.parens (pp e)
  pp (FCall name args) = PP.text name <> PP.parens (joinBy PP.comma (map pp args))
    where
      joinBy :: Doc -> [Doc] -> Doc
      joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty
  pp (ArrCons exps) = PP.brackets $ joinBy PP.comma (map pp exps)
    where
      joinBy :: Doc -> [Doc] -> Doc
      joinBy sep = foldr (\x acc -> if acc == PP.empty then PP.parens x else PP.parens x <> sep <> acc) PP.empty

instance PP VarDecl where
  pp (VDecl n b t) = if b then PP.text "shared" else PP.text "" <+> PP.text n <+> PP.char ':' <+> pp t

instance PP Statement where
  pp (Assign s e) = (pp s <+> PP.char '=' <+> pp e) <> PP.char ';'
  pp (Let v e) = PP.text "let " <> (pp v <+> PP.char '=' <+> pp e) <> PP.char ';'
  pp (If e b1 (Block [])) =
    PP.text "if" <> PP.parens (pp e) <> PP.text " {"
      PP.$$ PP.nest 4 (pp b1)
      PP.$$ PP.text "}"
  pp (If e b1 b2) =
    PP.text "if" <> PP.parens (pp e) <> PP.text " {"
      PP.$$ PP.nest 4 (pp b1)
      PP.$$ PP.text "} else {"
      PP.$$ PP.nest 4 (pp b2)
      PP.$$ PP.text "}"
  pp (Return e) = (PP.text "return" <+> pp e) <> PP.char ';'
  pp (FCallStatement name args) = PP.text name <> PP.parens (joinBy PP.comma (map pp args)) <> PP.char ';'
    where
      joinBy :: Doc -> [Doc] -> Doc
      joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty
  pp (While exp body) =
    PP.text "while " <> PP.parens (pp exp) <> PP.text " {"
      PP.$$ PP.nest 4 (pp body)
      PP.$$ PP.text "}"
  pp (Atomic block) = PP.text "atomic" <> PP.text " {" PP.$$ PP.nest 4 (pp block) PP.$$ PP.text "}"
  pp (ForIn vdecl exp body) =
    PP.text "for " <> PP.parens (pp vdecl <> PP.text " in " <> pp exp) <> PP.text " {"
      PP.$$ PP.nest 4 (pp body)
      PP.$$ PP.text "}"

instance PP FDecl where
  pp (FDecl name args returnType b) =
    PP.text "func " <> PP.text name <> PP.parens (joinBy PP.comma (map pp args)) <> PP.text " {"
      PP.$$ PP.nest 4 (pp b)
      PP.$$ PP.text "}"
    where
      joinBy :: Doc -> [Doc] -> Doc
      joinBy sep = foldr (\x acc -> if acc == PP.empty then x else x <> sep <> acc) PP.empty

instance PP Block where
  pp (Block stmts) = PP.vcat (map pp stmts)

instance PP Program where
  pp (Program fdecls main) = PP.vcat (map pp fdecls) PP.$$ pp main

----- Arbitrary Declarations -----
instance Arbitrary Bop where
  arbitrary =
    QC.elements
      [ Add,
        Sub,
        Mult,
        Div,
        Mod,
        And,
        Or,
        Gt,
        Ge,
        Eq,
        NEq,
        Lt,
        Le
      ]

instance Arbitrary Uop where
  arbitrary = QC.elements [Neg, Not]

instance Arbitrary BType where
  arbitrary =
    QC.oneof
      [ pure VoidT,
        pure BoolT,
        pure IntT,
        pure StringT,
        ArrayT <$> arbitrary
      ]

genString :: Gen String
genString = QC.vectorOf 5 $ QC.elements "abcdefg"

instance Arbitrary Value where
  -- We don't generate ArrayVals because they aren't parsed, only ArrayCons expressions are
  arbitrary =
    QC.oneof
      [ BoolVal <$> arbitrary,
        IntVal . abs <$> arbitrary,
        StringVal <$> genString
      ]

genSizedExp :: Int -> Gen Exp
genSizedExp 0 =
  QC.oneof
    [ Val <$> arbitrary,
      Var <$> genString
    ]
genSizedExp n =
  QC.oneof
    [ Val <$> arbitrary,
      Var <$> genString,
      ArrCons <$> QC.vectorOf 5 (genSizedExp (n `div` 2)),
      BOp <$> arbitrary <*> genSizedExp (n `div` 2) <*> genSizedExp (n `div` 2),
      UOp <$> arbitrary <*> genSizedExp (n `div` 2),
      FCall <$> genString <*> QC.vectorOf 5 (genSizedExp (n `div` 2))
    ]

instance Arbitrary Exp where
  arbitrary = genSizedExp 16
  shrink (ArrCons exps) = exps
  shrink (BOp _ e1 e2) = [e1, e2]
  shrink (UOp _ e) = [e]
  shrink (FCall _ exps) = exps
  shrink _ = []

instance Arbitrary VarDecl where
  arbitrary = VDecl <$> genString <*> QC.elements [False] <*> arbitrary

instance Arbitrary LValue where
  arbitrary = LVar <$> genString

genStatementMaxDepth :: Int -> Gen Statement
genStatementMaxDepth 0 =
  QC.oneof
    [ Let <$> arbitrary <*> arbitrary,
      Assign <$> arbitrary <*> arbitrary,
      Return <$> arbitrary
    ]
genStatementMaxDepth n =
  QC.oneof
    [ Let <$> arbitrary <*> arbitrary,
      Assign <$> arbitrary <*> arbitrary,
      If <$> (genSizedExp 4) <*> genBlockMaxDepth (n - 1) <*> genBlockMaxDepth (n - 1),
      Return <$> arbitrary,
      FCallStatement <$> genString <*> QC.vectorOf 4 (genSizedExp 2),
      While <$> (genSizedExp 4) <*> genBlockMaxDepth (n - 1),
      ForIn <$> arbitrary <*> genSizedExp 4 <*> genBlockMaxDepth (n - 1),
      Atomic <$> genBlockMaxDepth (n - 1)
    ]

instance Arbitrary Statement where
  arbitrary = genStatementMaxDepth 4
  shrink (If _ (Block b1) (Block b2)) = b1 ++ b2
  shrink (While _ (Block b)) = b
  shrink _ = []

genBlockMaxDepth :: Int -> Gen Block
genBlockMaxDepth n = Block <$> QC.vectorOf 5 (genStatementMaxDepth n)