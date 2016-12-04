module PrecompilerPrinter(
  OutputAssembly(..),
  showBytecode
) where
import Precompiler
import Text.PrettyPrint

--------------------------------------------------------------------------------
--                              EXPORTED                                      --
--------------------------------------------------------------------------------

data OutputAssembly = Linux | MacOS deriving Show

instance Show Bytecode where
  show p = showBytecode Linux p

showBytecode style p = render (prelude style $$ (programRepr style p) $$ emptyLine)

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------

idnt :: Int
idnt = 2

listRepr :: OutputAssembly -> (OutputAssembly -> a -> Doc) -> [a] ->  Doc
-- Get the doc representation of a list of elements of any type
listRepr style f ls = foldl ($$) empty (map (\x -> f style x) ls)

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

stmtRepr style (Mov reg n) = text "mov" <+> regRepr style reg <> comma <+> text (show n)
stmtRepr style (Movr reg reg2) = text "mov" <+> regRepr style reg <> comma <+> regRepr style reg2
stmtRepr style (Movtm mem reg) = text "mov" <+> memRepr style mem <> comma <+> regRepr style reg
stmtRepr style (Movfm reg mem) = text "mov" <+> regRepr style reg <> comma <+> memRepr style mem
stmtRepr style (CCall prim) = text "call" <+> primRepr style prim
stmtRepr style (Call name) = text "call" <+> text "cuca_" <> text name
stmtRepr style (Push n) = text "push" <+> text (show n)
stmtRepr style (Pushr reg) =  text "push" <+> regRepr style reg
stmtRepr style (Popr reg) =  text "pop" <+> regRepr style reg
stmtRepr style (Sub reg n) =  text "sub" <+> regRepr style reg <> comma <+> text (show n)
stmtRepr style (Add reg n) =  text "add" <+> regRepr style reg <> comma <+> text (show n)
stmtRepr style (Ret) = text "ret"

regRepr style LLIString = text "lli_format_string"
regRepr style Rdi = text "rdi"
regRepr style Rsi = text "rsi"
regRepr style Rax = text "rax"
regRepr style Rbx = text "rbx"
regRepr style Rcx = text "rcx"
regRepr style Rdx = text "rdx"
regRepr style R8 = text "r8"
regRepr style R9 = text "r9"
regRepr style R10 = text "r10"
regRepr style R11 = text "r11"
regRepr style R12 = text "r12"
regRepr style R13 = text "r13"
regRepr style R14 = text "r14"
regRepr style R15 = text "r15"
regRepr style Rbp = text "rbp"
regRepr style Rsp = text "rsp"

memRepr style (Mem r op n) = text "[" <> regRepr style r <+> text op <+> text (show n) <> text "]"
memRepr style (Memr r) = text "[" <> regRepr style r <> text "]"


primRepr style CCExit = text (exitFunction style)
primRepr style CCPutChar = text (putcharFunction style)
primRepr style CCPrintf = text (printfFunction style)
