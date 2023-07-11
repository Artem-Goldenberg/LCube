module Implementation where

import Data.Function (on)
import Control.Monad (when, guard)
type Symb = String

infixl 4 :@:
infixr 3 >-> -- теперь просто функция

data Expr = Idx Int
          | Ast
          | Box
          | Expr :@: Expr
          | Lmb Decl Expr
          | Pi Decl Expr    -- расширенный функциональный тип
    deriving (Read,Show,Eq,Ord)

data Decl = EDecl Symb Expr
    deriving (Read,Show,Ord)

instance Eq Decl where
  EDecl _ t1 == EDecl _ t2 = t1 == t2

type Env = [Decl]

type Rule = (Expr,Expr)

rulesS,rulesF,rulesO,rulesP,rulesFO,rulesPF,rulesPO,rulesPFO :: [Rule]
rulesS   = [(Ast,Ast)]
rulesF   = (Box,Ast) : rulesS
rulesO   = (Box,Box) : rulesS
rulesP   = (Ast,Box) : rulesS
rulesFO  = (Box,Ast) : rulesO
rulesPF  = (Ast,Box) : rulesF
rulesPO  = (Ast,Box) : rulesO
rulesPFO = (Ast,Box) : rulesFO

lE, pE :: Symb -> Expr -> Expr -> Expr
lE v = Lmb . EDecl v
pE v = Pi  . EDecl v

(>->) :: Expr -> Expr -> Expr
a >-> b = pE "_" a (shift 1 b)
----------------------

isKind :: Expr -> Bool
isKind Box = True
isKind Ast = True
isKind _   = False

validEnv :: [Rule] -> Env -> Bool
validEnv _ [] = True
validEnv rs ((EDecl _ tp):ctx)
    | Just k <- fer rs ctx tp, isKind k = validEnv rs ctx
    | otherwise = False

shift :: Int -> Expr -> Expr
shift = sh 0
  where
    sh m val (Idx n)
      | n < m = Idx n
      | otherwise = Idx $ n + val
    sh m val (t1 :@: t2) = sh m val t1 :@: sh m val t2
    sh m val (Lmb (EDecl sym e) t) = Lmb (EDecl sym $ sh m val e) $ sh (m + 1) val t
    sh m val (Pi (EDecl sym tp) e) = Pi (EDecl sym $ sh m val tp) $ sh (m + 1) val e
    sh m val Ast = Ast
    sh m val Box = Box

subst :: Int -> Expr -> Expr -> Expr
subst j s (Idx n)
  | n == j = s
  | n /= j = Idx n
subst j s (t1 :@: t2) = subst j s t1 :@: subst j s t2
subst j s (Lmb (EDecl sym e) t) = Lmb (EDecl sym $ subst j s e) $ subst (j + 1) (shift 1 s) t
subst j s (Pi (EDecl sym tp) e) = Pi (EDecl sym $ subst j s tp) $ subst (j + 1) (shift 1 s) e
subst _ _ Ast = Ast
subst _ _ Box = Box

fer :: [Rule] -> Env -> Expr -> Maybe Expr
fer _ _ Ast = Just Box
fer _ env (Idx n)
  | n < length env, EDecl _ a <- env !! n = Just $ shift (n + 1) a
fer rs env (Pi d@(EDecl sym a) b)
  | Just s1 <- fer rs env a, Just s2 <- fer rs (d:env) b, (s1, s2) `elem` rs = Just s2
fer rs env (m :@: n)
  | Just c  <- fer rs env m, (Pi (EDecl _ a) b) <- nf c,
        Just a' <- fer rs env n, nf a' == nf a = Just $ shift (-1) $ subst 0 (shift 1 n) b
fer rs env (Lmb d@(EDecl x a) m)
  | Just s1 <- fer rs env a, Just b <- fer rs (d:env) m, 
        Just s2 <- fer rs env (Pi (EDecl x a) b), (s1, s2) `elem` rs = Just $ Pi (EDecl x a) b
fer _ _ _ = Nothing

infer :: [Rule] -> Env -> Expr -> Maybe Expr
infer rs env e = do
    guard $ validEnv rs env
    fer rs env e

infer0 :: [Rule] -> Expr -> Maybe Expr
infer0 rs = infer rs []

oneStep :: Expr -> Maybe Expr
oneStep (t1 :@: t2)
  | Lmb (EDecl sym tp) t <- t1 = Just $ shift (-1) $ subst 0 (shift 1 t2) t
  | Just t <- oneStep t1 = Just $ t :@: t2
  | Just t <- oneStep t2 = Just $ t1 :@: t
oneStep (Pi (EDecl sym tp) e)
  | Just tp' <- oneStep tp = Just $ Pi (EDecl sym tp') e
  | Just e' <- oneStep e = Just $ Pi (EDecl sym tp) e'
oneStep (Lmb (EDecl sym e) t)
  | Just e' <- oneStep e = Just $ Lmb (EDecl sym e') t
  | Just t' <- oneStep t = Just $ Lmb (EDecl sym e) t'
oneStep _ = Nothing

nf :: Expr -> Expr
nf u = maybe u nf $ oneStep u