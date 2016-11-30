module PrecompilerPrinter where
import Precompiler
import Text.PrettyPrint

idnt :: Int
idnt = 2

listRepr :: (a -> Doc) -> [a] ->  Doc
-- Get the doc representation of a list of elements of any type
listRepr f ls = foldl ($$) empty (map (\x -> f x) ls)

instance Show Bytecode where
  show p = render (prelude $$ (programRepr p) $$ text "")

emptyLine = text ""

prelude = text "section .text" $$
          emptyLine $$
          text "global _main" $$
          emptyLine $$
          text "extern _exit, _putchar" $$
          emptyLine

programEnd = text "mov rdi, 0" $$ text "call _exit"

function name contents = text (name ++ ":") $$ (nest idnt contents)

programRepr BytecodeEmptyProgram = function "_main" programEnd
programRepr (BytecodeProgram funcs) = listRepr funcRepr funcs $$ function "_main" (text "call cuca_main" $$ programEnd)

funcRepr (BFunc name stmts) = function name ((listRepr stmtsRepr stmts) $$ text "ret" $$ emptyLine)

stmtsRepr _ = empty
