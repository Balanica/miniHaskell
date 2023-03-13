
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar v = show v


inParens :: String -> String
inParens s = "(" ++ s ++ ")"

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat v) = show v
showExp (CLam v cexp) = inParens ("\\ " ++ (showVar v) ++ "->" ++ (showExp cexp))
showExp (CApp c1 c2) = inParens ((showExp c1) ++ " " ++ (showExp c2))
showExp (Let v c1 c2) = inParens("let " ++ (showVar v) ++ " := " ++ (showExp c1) ++ " in" ++ (showExp c2))
showExp (LetRec v c1 c2) = inParens("letrec " ++ (showVar v) ++ " := " ++ (showExp c1) ++ " in" ++ (showExp c2))
showExp (List l) = "[" ++ intercalate ", " (map showExp l) ++ "]"
