module AssemblyPrinter(
  OutputAssembly(..),
  showBytecode
) where
import Assembler
import Text.PrettyPrint

--------------------------------------------------------------------------------
--                              EXPORTED                                      --
--------------------------------------------------------------------------------

data OutputAssembly = Linux | MacOS | Windows deriving Show

instance Show Bytecode where
  show p = showBytecode Linux p

showBytecode :: OutputAssembly -> Bytecode -> String
showBytecode style p = render (prelude style $$ (programRepr style p) $$ emptyLine)

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
prelude :: OutputAssembly -> Doc
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

programRepr :: OutputAssembly -> Bytecode -> Doc
programRepr style BytecodeEmptyProgram = function (mainFunction style) (stmtsRepr style [Mov Rdi $ Const 0, CCall CCExit])
programRepr style (BytecodeProgram funcs) =
  listRepr style funcRepr funcs $$
  function (mainFunction style) (stmtsRepr style [Call "cuca_main", Mov Rdi $ Const 0, CCall CCExit])

funcRepr :: OutputAssembly -> BytecodeFunc -> Doc
funcRepr style (BFunc name stmts) = function name (stmtsRepr style stmts $$ emptyLine)

stmtsRepr :: OutputAssembly -> [BytecodeStmt] -> Doc
stmtsRepr style stmts = listRepr style stmtRepr stmts

stmtRepr :: OutputAssembly -> BytecodeStmt -> Doc
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
stmtRepr style (MovQ reg1 reg2) = binaryRegistryOp style "mov qword" reg1 reg2
stmtRepr style (Add reg1 (Const 0)) = empty
stmtRepr style (Add reg1 reg2) = binaryRegistryOp style "add" reg1 reg2
stmtRepr style (Sub reg1 (Const 0)) = empty
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
stmtRepr style (Inc reg) = unaryRegistryOp style "inc" reg
stmtRepr style (Sal reg1 reg2) = binaryRegistryOp style "sal" reg1 reg2

regRepr :: OutputAssembly -> BytecodeValue -> Doc
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
regRepr style (Mem r op 0) = text "[" <> regRepr style r <> text "]"
regRepr style (Mem r op n) = text "[" <> regRepr style r <+> text op <+> text (show n) <> text "]"
regRepr style (Const n) = text (show n)

primRepr :: OutputAssembly -> BytecodePrimitive -> Doc
primRepr style CCExit = text (exitFunction style)
primRepr style CCPutChar = text (putcharFunction style)
primRepr style CCPrintf = text (printfFunction style)

--------------------------------------------------------------------------------
--                              HELPERS                                       --
--------------------------------------------------------------------------------
idnt :: Int
idnt = 2

listRepr :: OutputAssembly -> (OutputAssembly -> a -> Doc) -> [a] ->  Doc
-- Get the doc representation of a list of elements of any type
listRepr style f ls = foldl ($$) empty (map (\x -> f style x) ls)

emptyLine :: Doc
emptyLine = text ""

function :: String -> Doc -> Doc
function name contents = text name <> colon $$ (nest idnt contents)

unaryRegistryOp :: OutputAssembly -> String -> BytecodeValue -> Doc
unaryRegistryOp style name reg = text name <+> regRepr style reg

binaryRegistryOp :: OutputAssembly -> String -> BytecodeValue -> BytecodeValue -> Doc
binaryRegistryOp style name reg1 reg2 = text name <+> regRepr style reg1 <> comma <+> regRepr style reg2

--------------------------------------------------------------------------------
--                              OS DEPENDENT                                  --
--------------------------------------------------------------------------------

mainFunction :: OutputAssembly -> String
mainFunction Linux = "main"
mainFunction MacOS = "_main"
mainFunction Windows = "_main"

exitFunction :: OutputAssembly -> String
exitFunction Linux = "exit"
exitFunction MacOS = "_exit"
exitFunction Windows = "_exit"

putcharFunction :: OutputAssembly -> String
putcharFunction Linux = "putchar"
putcharFunction MacOS = "_putchar"
putcharFunction Windows = "_putchar"

printfFunction :: OutputAssembly -> String
printfFunction Linux = "printf"
printfFunction MacOS = "_printf"
printfFunction Windows = "_printf"
