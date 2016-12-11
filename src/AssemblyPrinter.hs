module AssemblyPrinter(
  OutputAssembly(..),
  showBytecode
) where
import Assembler
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

function name contents = text name <> colon $$ (nest idnt contents)

programRepr style BytecodeEmptyProgram = function (mainFunction style) (stmtsRepr style [Mov Rdi $ Const 0, CCall CCExit])
programRepr style (BytecodeProgram funcs) =
  listRepr style funcRepr funcs $$
  function (mainFunction style) (stmtsRepr style [Call "cuca_main", Mov Rdi $ Const 0, CCall CCExit])

funcRepr style (BFunc name stmts) = function name (stmtsRepr style stmts $$ emptyLine)

stmtsRepr style stmts = listRepr style stmtRepr stmts

stmtRepr style (Comment comment) = semi <+> text comment
stmtRepr style (Commented comment stmt) = stmtRepr style stmt <+> semi <+> text comment
stmtRepr style (EmptyLine) = emptyLine
stmtRepr style (Label name) = text name <> colon
stmtRepr style (CCall prim) = text "call" <+> primRepr style prim
stmtRepr style (Call name) = text "call" <+> text name
stmtRepr style (Ret) = text "ret"
stmtRepr style (Push reg) = unaryRegistryOp style "push" reg
stmtRepr style (Pop reg) =  unaryRegistryOp style "pop" reg
stmtRepr style (Mov reg1 reg2) = binaryRegistryOp style "mov" reg1 reg2
stmtRepr style (Add reg1 reg2) = binaryRegistryOp style "add" reg1 reg2
stmtRepr style (Sub reg1 reg2) = binaryRegistryOp style "sub" reg1 reg2
stmtRepr style (Mul reg) = unaryRegistryOp style "imul" reg
stmtRepr style (And reg1 reg2) = binaryRegistryOp style "and" reg1 reg2
stmtRepr style (Or reg1 reg2) = binaryRegistryOp style "or" reg1 reg2
stmtRepr style (Not reg) = unaryRegistryOp style "not" reg
stmtRepr style (Cmp reg1 reg2) = binaryRegistryOp style "cmp" reg1 reg2
stmtRepr style (Jmp name) = text "jmp" <+> text name
stmtRepr style (Je name) = text "je" <+> text name
stmtRepr style (Jne name) = text "jne" <+> text name
stmtRepr style (Jle name) = text "jle" <+> text name
stmtRepr style (Jge name) = text "jge" <+> text name
stmtRepr style (Jl name) = text "jl" <+> text name
stmtRepr style (Jg name) = text "jg" <+> text name

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
regRepr style (Mem r op n) = text "[" <> regRepr style r <+> text op <+> text (show n) <> text "]"
regRepr style (Const n) = text (show n)

primRepr style CCExit = text (exitFunction style)
primRepr style CCPutChar = text (putcharFunction style)
primRepr style CCPrintf = text (printfFunction style)

unaryRegistryOp style name reg = text name <+> regRepr style reg
binaryRegistryOp style name reg1 reg2 = text name <+> regRepr style reg1 <> comma <+> regRepr style reg2
