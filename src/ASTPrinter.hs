module ASTPrinter(stringify) where
import Parser

stringify :: ProgramT -> String
stringify _  = ""
-- stringify EmptyProgram    = ""
-- stringify (Program funcs) = "(Program\n" ++ strMany 1 funcs ++ "\n)"

{-}
nodeRepr :: Integer -> a -> [String]
nodeRepr n x = case x of
    Function ident typ param block -> ["Function", ident, show typ]--, strMany n param, strOne n block]
    Parameter ident typ -> ["Parameter"]
    _ -> ["Otro"]



indent :: Integer -> String
indent 0 = ""
indent n = "  " ++ indent (n-1)

strMany :: Integer -> [a] -> String
strMany n [] = ""
strMany n (x:xs) = strOne n x ++ strMany n xs

strOne :: Integer -> a -> String
strOne n x = indent n ++ "(" ++ strElem n x ++ "\n" ++ indent n ++ ")"

strElem :: Integer -> a -> String
strElem n e = strAll n (nodeRepr n e)

strAll :: Integer -> [String] -> String
strAll n ([]) = ""
strAll n (x:[]) = x
strAll n (x:xs) = x ++ "\n" ++ indent (n+1) ++ strAll n xs
-}
