module Interpreter( eval) where
import Grammar
import Lexer

type Env = String -> Exp

emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

doEval :: Exp -> Env -> Int
doEval (Int v) _         = v
doEval (Plus e1 e2) env  = (doEval e1 env) + (doEval e2 env)
doEval (Minus e1 e2) env = (doEval e1 env) - (doEval e2 env)
doEval (Times e1 e2) env = (doEval e1 env) * (doEval e2 env)
doEval (Div e1 e2) env   = (doEval e1 env) `div` (doEval e2 env)
doEval (Negate e) env    = -(doEval e env)
doEval (Var s) env       = doEval (envLookup s env) env
doEval (Let s e1 e2) env = doEval e2 env'
    where env' = envBind s e1 env

eval :: Exp -> Int
eval e = doEval e emptyEnv
