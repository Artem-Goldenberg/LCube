module Examples where

import Implementation

tArr  = Idx 0 >-> Idx 0
tBool = Idx 0 >-> Idx 0 >-> Idx 0
tNat  = tArr >-> tArr
tK    = Idx 1 >-> Idx 0 >-> Idx 1
tKast = Idx 1 >-> Idx 0 >-> Idx 0
-- Комбинатор $I$ (Idx 0 в cIFopen ссылается в никуда, нужен контекст)
cIFopen = lE "x" (Idx 0) $ Idx 0  
cIF = lE "a" Ast $ lE "x" (Idx 0) $ Idx 0
-- Комбинаторы $K$ и $K_\ast$
tKF = pE "a" Ast $ pE  "b" Ast tK
cKF = lE "a" Ast $ lE "b" Ast $ lE "x" (Idx 1) $ lE "y" (Idx 1) $ Idx 1
tKastF = pE "a" Ast $ pE "b" Ast tKast
cKastF = lE "a" Ast $ lE "b" Ast $ lE "x" (Idx 1) $ lE "y" (Idx 1) $ Idx 0
-- Комбинатор $C$ 
tFlip = (Idx 2 >-> Idx 1 >-> Idx 0) >-> Idx 1 >-> Idx 2 >-> Idx 0
tFlipF = pE "a" Ast $ pE "b" Ast $ pE "c" Ast $ tFlip
cFlipF = lE "a" Ast $ lE "b" Ast $ lE "c" Ast $ lE "f" (Idx 2 >-> Idx 1 >-> Idx 0) $ lE "y" (Idx 2) $ lE "x" (Idx 4) $ Idx 2 :@: Idx 0 :@: Idx 1
-- Кодирование булевых значений в System F
boolT = pE "a" Ast $ Idx 0 >-> Idx 0 >-> Idx 0
fls = lE "a" Ast $ lE "t" (Idx 0) $ lE "f" (Idx 1) $ Idx 0
tru = lE "a" Ast $ lE "t" (Idx 0) $ lE "f" (Idx 1) $ Idx 1
ifThenElse = lE "a" Ast $ lE "v" boolT $ lE "x" (Idx 1) $ lE "y" (Idx 2) $ Idx 2 :@: Idx 3 :@: Idx 1 :@: Idx 0
notF = lE "v" boolT $ lE "a" Ast $ lE "t" (Idx 0) $ lE "f" (Idx 1) $ Idx 3 :@: Idx 2 :@: Idx 0 :@: Idx 1
-- Кодирование самоприменения в System F (примеры из лекции)
botF = pE "a" Ast (Idx 0)
topF = pE "a" Ast tArr
sa0 = lE "z" botF $ lE "b" Ast $ Idx 1 :@: (Idx 0 >-> Idx 0) :@: (Idx 1 :@: Idx 0)
sa1 = lE "z" topF $ lE "b" Ast $ Idx 1 :@: (Idx 0 >-> Idx 0) :@: (Idx 1 :@: Idx 0)
sa2 = lE "z" topF $ Idx 0 :@: topF :@: Idx 0
-- Кодирование натуральных чисел в System F
natT = pE "a" Ast $ (Idx 0 >-> Idx 0) >-> Idx 0 >-> Idx 0
natAbs body = lE "a" Ast $ lE "s" (Idx 0 >-> Idx 0) $ lE "z" (Idx 1) body
zero  = natAbs $ Idx 0
one   = natAbs $ Idx 1 :@: Idx 0
two   = natAbs $ Idx 1 :@: (Idx 1 :@: Idx 0)
three = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0))
four  = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0)))
five  = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0))))
six   = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0)))))
seven = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0))))))
eight = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0)))))))
nine  = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0))))))))
ten   = natAbs $ Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: (Idx 1 :@: Idx 0)))))))))
isZeroF = lE "m" natT $ Idx 0 :@: boolT :@: lE "x" boolT fls :@: tru
succF = lE "m" natT $ lE "b" Ast $ lE "s" (Idx 0 >-> Idx 0) $ lE "z" (Idx 1) $ Idx 1 :@: (Idx 3 :@: Idx 2 :@: Idx 1 :@: Idx 0)
-- кодирование вектора в Calculus of Construction
vecT = lE "sigma" Ast
     $ lE "m" natT
     $ pE "phi" (natT >-> Ast) 
     $ (Idx 0 :@: zero) 
        >-> (pE "n" natT $ Idx 3 >-> Idx 1 :@: Idx 0 >-> Idx 1 :@: (succF :@: Idx 0)) 
        >-> Idx 0 :@: Idx 1
nil = lE "sigma" Ast 
    $ lE "phi" (natT >-> Ast) 
    $ lE "z" (Idx 0 :@: zero) 
    $ lE "c" (pE "n" natT $ Idx 3 >-> Idx 2 :@: Idx 0 >-> Idx 2 :@: (succF :@: Idx 0)) 
    $ (Idx 1)
nilT = nf 
     $ pE "sigma" Ast 
     $ vecT :@: Idx 0 :@: zero
cons = lE "sigma" Ast 
     $ lE "n" natT 
     $ lE "e" (Idx 1) 
     $ lE "v" (vecT :@: Idx 2 :@: Idx 1) 
     $ lE "phi" (natT >-> Ast) 
     $ lE "z" (Idx 0 :@: zero) 
     $ lE "c" (pE "n" natT $ Idx 6 >-> Idx 2 :@: Idx 0 >-> Idx 2 :@: (succF :@: Idx 0)) 
     $ Idx 0 :@: Idx 5 :@: Idx 4 :@: (Idx 3 :@: Idx 2 :@: Idx 1 :@: Idx 0) 
consT = nf 
      $ pE "sigma" Ast 
      $ pE "n" natT 
      $ Idx 1 
        >-> vecT :@: Idx 1 :@: Idx 0
        >-> vecT :@: Idx 1 :@: (succF :@: Idx 0)
vecNatT = nf $ vecT :@: natT
vec12 = cons 
    :@: natT -- тип 
    :@: one  -- индекс
    :@: one  -- элемент
    :@: (cons 
           :@: natT -- тип 
           :@: zero -- индекс
           :@: two  -- элемент
           :@: (nil :@: natT))