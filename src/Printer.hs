module Printer(
  programRepr
) where
import Parser
import Text.PrettyPrint

--------------------------------------------------------------------------------
--                              EXPORTED                                      --
--------------------------------------------------------------------------------
instance Show ProgramT where
  show p = render $ (programRepr idnt p) $$ text ""

programRepr :: Int -> ProgramT -> Doc
programRepr n EmptyProgram = programRepr n $ Program []
programRepr n (Program funcs) =
  parenthesis (
    text      "Program"           $$
    listRepr  funcRepr funcs n
  )

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
funcRepr :: FunctionT -> Int -> Doc
funcRepr (Function name typ params block) n =
  nestify n "Function" [nested name, typeRepr typ, listRepr paramRepr params, blockRepr block]

typeRepr :: Type -> Int -> Doc
typeRepr typ n = nested (show typ) n

paramRepr :: ParameterT -> Int -> Doc
paramRepr (Parameter name typ) n =
  nestify n "Parameter" [nested name, typeRepr typ]

blockRepr :: BlockT -> Int -> Doc
blockRepr (Block stmts) n =
  nestify n "Block" [listRepr stmtRepr stmts]

stmtRepr :: StmtT -> Int -> Doc
stmtRepr (StmtAssign name expr) n =
  nestify n "StmtAssign" [nested name, exprRepr expr]
stmtRepr (StmtVecAssign name exprL exprR) n =
  nestify n "StmtVecAssign" [nested name, exprRepr exprL, exprRepr exprR]
stmtRepr (StmtIf expr block) n =
  nestify n "StmtIf" [exprRepr expr, blockRepr block]
stmtRepr (StmtIfElse expr blockIf blockElse) n =
  nestify n "StmtIfElse" [exprRepr expr, blockRepr blockIf, blockRepr blockElse]
stmtRepr (StmtWhile expr block) n =
  nestify n "StmtWhile" [exprRepr expr, blockRepr block]
stmtRepr (StmtReturn expr) n =
  nestify n "StmtReturn" [exprRepr expr]
stmtRepr (StmtCall name exprsns) n =
  nestify n "StmtCall" [nested name, listRepr exprRepr exprsns]

exprRepr :: ExprT -> Int -> Doc
exprRepr (ExprVar name) n =
  nestify n "ExprVar" [nested name]
exprRepr (ExprConstNum val) n =
  nestify n "ExprConstNum" [nested (show val)]
exprRepr (ExprConstBool val) n =
  nestify n "ExprConstBool" [nested (show val)]
exprRepr (ExprVecMake exprsns) n =
  nestify n "ExprVecMake" [listRepr exprRepr exprsns]
exprRepr (ExprVecLength name) n =
  nestify n "ExprVecLength" [nested name]
exprRepr (ExprVecDeref name expr) n =
  nestify n "ExprVecDeref" [nested name, exprRepr expr]
exprRepr (ExprCall name exprsns) n =
  nestify n "ExprCall" [nested name, listRepr exprRepr exprsns]
exprRepr (ExprNot expr) n =
  nestify n "ExprNot" [exprRepr expr]
exprRepr (ExprAnd exprL exprR) n =
  nestify n "ExprAnd" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprOr exprL exprR) n =
  nestify n "ExprOr" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprLe exprL exprR) n =
  nestify n "ExprLe" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprGe exprL exprR) n =
  nestify n "ExprGe" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprLt exprL exprR) n =
  nestify n "ExprLt" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprGt exprL exprR) n =
  nestify n "ExprGt" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprEq exprL exprR) n =
  nestify n "ExprEq" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprNe exprL exprR) n =
  nestify n "ExprNe" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprAdd exprL exprR) n =
  nestify n "ExprAdd" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprSub exprL exprR) n =
  nestify n "ExprSub" [exprRepr exprL, exprRepr exprR]
exprRepr (ExprMul exprL exprR) n =
  nestify n "ExprMul" [exprRepr exprL, exprRepr exprR]

--------------------------------------------------------------------------------
--                               HELPERS                                      --
--------------------------------------------------------------------------------
idnt :: Int
idnt = 1

nested :: String -> Int -> Doc
-- Return a doc of the given string nested a given amount of places to the right
nested str n = nest n $ text str

parenthesis :: Doc -> Doc
-- Return the document wrapped in parentesis with the expected newlines added
parenthesis x = lparen <> x $$ rparen

nestedParens :: Int -> Doc -> Doc
-- Return the document indented by a given space and wrapped around the expected parens
nestedParens n x = nest n $ parenthesis x

listRepr :: (a -> Int -> Doc) -> [a] -> Int ->  Doc
-- Get the doc representation of a list of elements of any type
listRepr f ls n = foldl ($$) empty (map (\x -> f x n) ls)

nestify :: Int -> String -> [(Int -> Doc)] -> Doc
nestify n name elems = nestedParens n $ foldl (\a e -> a $$ (e n) ) (text name) elems
