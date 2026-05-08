module Prop where

import Data.Void (Void)

data PropExpr
  = CTrue
  | CFalse
  | Variable String
  | Neg (PropExpr)
  | PropExpr :&: PropExpr
  | PropExpr :|: PropExpr
  | PropExpr :=>: PropExpr
  deriving Show

infixl 9 :&:
infixl 8 :|:
infixr 7 :=>:

expr1 :: PropExpr
expr1 = CTrue :&: (Variable "p" :&: CTrue) :|: CFalse :=>: Variable "p"

constLawsHelper :: PropExpr -> PropExpr
constLawsHelper (CTrue :&: rhs) = rhs
constLawsHelper (lhs :&: CTrue) = lhs
constLawsHelper (CFalse :|: rhs) = rhs
constLawsHelper (lhs :|: CFalse) = lhs
constLawsHelper p = p

constLaws :: PropExpr -> PropExpr
constLaws (Neg p) = Neg $ constLaws p
constLaws (lhs :&: rhs) =
  constLawsHelper (constLaws lhs :&: constLaws rhs)
constLaws (lhs :|: rhs) =
  constLawsHelper (constLaws lhs :|: constLaws rhs)
constLaws (lhs :=>: rhs) =
  constLawsHelper (constLaws lhs :=>: constLaws rhs)
constLaws p = p

-- >>> constLaws expr1
-- Variable "p" :=>: Variable "p"

expr2 :: PropExpr
expr2 = Neg (Neg (Neg (Variable "p"))) :&: (Neg (Neg $ Variable "a") :=>: Variable "a")

excludedMiddleHelper :: PropExpr -> PropExpr
excludedMiddleHelper (Neg (Neg p)) = p
excludedMiddleHelper p = p

excludedMiddle :: PropExpr -> PropExpr
excludedMiddle (Neg p) = excludedMiddleHelper $ Neg $ excludedMiddle p
excludedMiddle (lhs :&: rhs) = excludedMiddle lhs :&: excludedMiddle rhs
excludedMiddle (lhs :|: rhs) = excludedMiddle lhs :|: excludedMiddle rhs
excludedMiddle (lhs :=>: rhs) = excludedMiddle lhs :=>: excludedMiddle rhs
excludedMiddle p = p

-- >>> excludedMiddle expr2
-- Neg (Variable "p") :&: (Variable "a" :=>: Variable "a")

expr3 :: PropExpr
expr3 = Neg $ Variable "p" :|: Neg (Variable "p" :&: CTrue)

deMorganHelper :: PropExpr -> PropExpr
deMorganHelper (Neg (lhs :&: rhs)) = Neg lhs :|: Neg rhs
deMorganHelper (Neg (lhs :|: rhs)) = Neg lhs :&: Neg rhs
deMorganHelper p = p

deMorgan :: PropExpr -> PropExpr
deMorgan (Neg p) = deMorganHelper $ Neg $ deMorgan p
deMorgan (lhs :&: rhs) = deMorgan lhs :&: deMorgan rhs
deMorgan (lhs :|: rhs) = deMorgan lhs :|: deMorgan rhs
deMorgan (lhs :=>: rhs) = deMorgan lhs :=>: deMorgan rhs
deMorgan p = p

-- >>> deMorgan expr3
-- Neg (Variable "p") :&: Neg (Neg (Variable "p") :|: Neg CTrue)

simplify :: PropExpr -> PropExpr
simplify (Neg CTrue) = CFalse
simplify (Neg CFalse) = CTrue
simplify (Neg p) = Neg $ simplify p
simplify (lhs :&: rhs) = simplify lhs :&: simplify rhs
simplify (lhs :|: rhs) = simplify lhs :|: simplify rhs
simplify (lhs :=>: rhs) = simplify lhs :=>: simplify rhs
simplify p = p

-- >>> simplify $ deMorgan expr3
-- Neg (Variable "p") :&: Neg (Neg (Variable "p") :|: CFalse)

type Env k v = [(k, v)]

-- ефективно така "повдигаме" двуаргументна функция
-- от тип f :: a -> b -> c
-- до функция от тип
-- liftedF :: Maybe a -> Maybe b -> Maybe c
-- т.е. можем да прилагаме "чисти" функции
-- върху две Maybe стойности
liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe op (Just x) (Just y) = Just $ x `op` y
liftMaybe _ _ _ = Nothing

eval :: Env String Bool -> PropExpr -> Maybe Bool
eval _ CTrue = Just True
eval _ CFalse = Just False
eval ctx (Variable x) = case lookup x ctx of
  Nothing -> Nothing
  Just valuation -> Just valuation
-- можем да си направим и помощна функция,
-- която да прилага едноместна функция
-- върху стойността в Maybe
eval ctx (Neg p) = case eval ctx p of
  Nothing -> Nothing
  Just valuation -> Just $ not valuation
eval ctx (lhs :&: rhs) = liftMaybe (&&) (eval ctx lhs) (eval ctx rhs)
eval ctx (lhs :|: rhs) = liftMaybe (||) (eval ctx lhs) (eval ctx rhs)
eval ctx (lhs :=>: rhs) = liftMaybe implToDisj (eval ctx lhs) (eval ctx rhs)
 where
  -- идва от еквивалентността
  -- p => q   ===   ~p v q
  implToDisj :: Bool -> Bool -> Bool
  implToDisj p q = not p || q

expr4 :: PropExpr
expr4 = Variable "a" :|: (Variable "b" :&: Variable "c" :=>: Neg (Variable "a"))

ctx1 :: Env String Bool
ctx1 = [("a", False), ("b", True), ("c", False)]

-- >>> eval ctx1 expr4
-- Just True

-- >>> eval ctx1 $ CFalse :&: expr4
-- Just False

-- >>> eval ctx1 $ expr4 :=>: Variable "d"
-- Nothing
