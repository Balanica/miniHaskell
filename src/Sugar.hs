module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var x) =  (makeIndexedVar x)

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar x 0) = Var x  
sugarVar (IndexedVar x i) = Var ( x ++ "_" ++ show(i))

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.

desugarExp :: ComplexExp -> Exp
desugarExp (CX var) = X (desugarVar var) 
desugarExp (CLam var exp) = Lam (desugarVar var) (desugarExp exp)
desugarExp (CApp exp1 exp2) = App (desugarExp exp1) (desugarExp exp2)
desugarExp (List []) = nilExp
desugarExp (List (h:t)) = App (App (consExp) (desugarExp h)) (App (consExp) (desugarExp (List t )))
desugarExp (Nat 0) = zeroExp
desugarExp (Nat n) = App (succExp) (desugarExp (Nat (n-1)))
desugarExp (Let v exp1 exp2) = App(Lam (desugarVar v) (desugarExp exp1)) (desugarExp exp2)
desugarExp (LetRec v exp1 exp2) = App (Lam (desugarVar v) ((desugarExp exp2))) (App (fixExp) (Lam (desugarVar v) (desugarExp exp1)))

-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp (X i) = CX (sugarVar i)
sugarExp (Lam i exp) = CLam (sugarVar i) (sugarExp exp)
sugarExp (App exp1 exp2) = CApp (sugarExp exp1) (sugarExp exp2)

-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
