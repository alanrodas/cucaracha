module PrecompilerPrinter where
import Precompiler
import Text.PrettyPrint

data OutputAssembly = Linux | MacOS

instance Show Bytecode where
  show p = showBytecode Linux p

idnt :: Int
idnt = 2

listRepr :: OutputAssembly -> (OutputAssembly -> a -> Doc) -> [a] ->  Doc
-- Get the doc representation of a list of elements of any type
listRepr style f ls = foldl ($$) empty (map (\x -> f style x) ls)

showBytecode style p = render (prelude style $$ (programRepr style p) $$ emptyLine)

emptyLine = text ""

mainFunction Linux = "main"
mainFunction MacOS = "_main"

exitFunction Linux = "exit"
exitFunction MacOS = "_exit"

putcharFunction Linux = "putchar"
putcharFunction MacOS = "_putchar"

printfFunction Linux = "printf"
printfFunction MacOS = "_printf"

prelude style = text "section .data" $$
          (regRepr style LLIString) <+> text "db \"%lli\"" $$
          emptyLine $$
          text "section .text" $$
          emptyLine $$
          text "global" <+> text (mainFunction style)  $$
          emptyLine $$
          text "extern" <+> text (exitFunction style) <> comma <+>
                            text (putcharFunction style) <> comma <+>
                            text (printfFunction style) $$
          emptyLine

function name contents = text (name ++ ":") $$ (nest idnt contents)

programRepr style BytecodeEmptyProgram = function (mainFunction style) (stmtsRepr style [Mov Rdi 0, CCall CCExit])
programRepr style (BytecodeProgram funcs) =
  listRepr style funcRepr funcs $$
  function (mainFunction style) (stmtsRepr style [Call "main", Mov Rdi 0, CCall CCExit])

funcRepr style (BFunc name stmts) = function name (stmtsRepr style stmts $$ emptyLine)

stmtsRepr style stmts = listRepr style stmtRepr stmts

stmtRepr style (Mov reg int) = text "mov" <+> regRepr style reg <> comma <+> text (show int)
stmtRepr style (Movr reg reg2) = text "mov" <+> regRepr style reg <> comma <+> regRepr style reg2
stmtRepr style (CCall prim) = text "call" <+> primRepr style prim
stmtRepr style (Call name) = text "call" <+> text "cuca_" <> text name
stmtRepr style (Ret) = text "ret"

regRepr style Rdi = text "rdi"
regRepr style Rsi = text "rsi"
regRepr style Rax = text "rax"
regRepr style LLIString = text "lli_format_string"

primRepr style CCExit = text (exitFunction style)
primRepr style CCPutChar = text (putcharFunction style)
primRepr style CCPrintf = text (printfFunction style)
