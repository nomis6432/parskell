{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module OldStuff.Syntax.Types where

-- | Recursive types and structurally recursive functions

data Expr f = In (f (Expr f))

fold :: Functor f => (f a -> a) -> (Expr f -> a)
fold alg (In t) = alg (fmap (fold alg) t)

-- | Co-products (sums) of functors


data (f :+: g) a = Inl (f a) | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl t) = Inl (fmap f t)
  fmap f (Inr t) = Inr (fmap f t)

-- | Functor inclusion

class (Functor f, Functor g) => f <: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)

instance Functor f => f <: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f <: (f :+: g) where
  inj = Inl
  prj (Inl t) = Just t
  prj (Inr t) = Nothing

instance (Functor g, f <: h) => f <: (g :+: h) where
  inj = Inr . inj
  prj (Inl t) = Nothing
  prj (Inr t) = prj t

inject :: (f <: g) => f (Expr g) -> Expr g
inject = In . inj

match :: (g <:f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

-- | syntax name

class Functor f => SyntaxName f where
  syntaxAlg :: f String -> String

instance (SyntaxName f1, SyntaxName f2) => SyntaxName (f1 :+: f2) where
  syntaxAlg (Inl t) = syntaxAlg t
  syntaxAlg (Inr t) = syntaxAlg t

getSyntax :: SyntaxName f => Expr f -> String
getSyntax = fold syntaxAlg

-- | Literals

data IntE a = IntE Int
  deriving (Functor, Show)

intE :: (IntE <: f) => Int -> Expr f
intE n = inject (IntE n)

instance SyntaxName IntE where
  syntaxAlg (IntE n) = show n

data CharE a = CharE Char
  deriving (Functor, Show)

charE :: (CharE <: f) => Char -> Expr f
charE n = inject (CharE n)

instance SyntaxName CharE where
  syntaxAlg (CharE n) = ['\'', n, '\'']

data ListE a = ListE [a]
  deriving (Functor, Show)

listE :: (ListE <: f) => [Expr f] -> Expr f
listE n = inject (ListE n)

printList :: [String] -> String
printList xs = "[" ++ commaSep xs ++ "]" where
  commaSep (x:y:xs) = x ++ ", " ++ commaSep (y:xs)
  commaSep [x] = x
  commasep [] = ""

instance SyntaxName ListE where
  syntaxAlg (ListE n) = printList n

data VarE a = VarE String
  deriving (Functor, Show)

varE :: (VarE <: f) => String -> Expr f
varE x = inject (VarE x)

instance SyntaxName VarE where
  syntaxAlg (VarE n) = n

-- | Functions

data FuncE a = FuncE a [a]
  deriving (Functor, Show)

funcE :: (FuncE <: f) => Expr f -> [Expr f] -> Expr f
funcE t xs = inject (FuncE t xs)

{-
instance (FuncE <: h) => Eval FuncE h where
  evalAlg (FuncE (Just a1) args) = add a1 a2 where
    add :: (AddE <:h, IntE <: h) => Expr h -> Expr h -> Maybe (Expr h)
    add a1 a2 = do
      IntE b1 <- match a1
      IntE b2 <- match a2
      return (intE (b1 + b2))
-}

-- | operators

data AddE a = AddE [a]
  deriving (Functor, Show)

addE :: (AddE <: f) => [Expr f] -> Expr f
addE n1 = inject (AddE n1)

instance SyntaxName AddE where
  syntaxAlg (AddE [x, y]) = "(+) " ++ x ++ " " ++ y

data SumE a = SumE
  deriving (Functor, Show)

sumE :: (SumE <: f) => Expr f
sumE = inject SumE

instance SyntaxName SumE where
  syntaxAlg SumE = "sum"

data ZipWithE a = ZipWithE
  deriving (Functor, Show)

zipWithE :: (ZipWithE <: f) => Expr f
zipWithE = inject ZipWithE

instance SyntaxName ZipWithE where
  syntaxAlg ZipWithE = "zipWith"

data SquareE a = SquareE
  deriving (Functor, Show)

squareE :: (SquareE <: f) => Expr f
squareE = inject SquareE

instance SyntaxName SquareE where
  syntaxAlg SquareE = "(^)"

-- | evaluating

class Functor f => Eval f h where
  evalAlg :: f (Maybe (Expr h)) -> Maybe (Expr h)

instance (Eval f1 h, Eval f2 h) => Eval (f1 :+: f2) h where
  evalAlg (Inl t) = evalAlg t
  evalAlg (Inr t) = evalAlg t

evaluate :: Eval f h => Expr f -> Maybe (Expr h)
evaluate = fold evalAlg

instance (IntE <: h) => Eval IntE h where
  evalAlg (IntE n) = Just (intE n)

instance (AddE <: h, IntE <: h) => Eval AddE h where
  evalAlg (AddE [Just a1, Just a2]) = add a1 a2 where
    add :: (AddE <:h, IntE <: h) => Expr h -> Expr h -> Maybe (Expr h)
    add a1 a2 = do
      IntE b1 <- match a1
      IntE b2 <- match a2
      return (intE (b1 + b2))
  evalAlg _ = Nothing

{-
instance (ZipWithE <: h) => Eval ZipWithE h where
  evalAlg (ZipWithE [f, Just a1, Just a2]) =

testExpr :: Expr (AddE :+: IntE)
testExpr = addE [intE 1, intE 2]

evaluateExpr :: Maybe (Expr (AddE :+: IntE))
evaluateExpr = evaluate testExpr

runTest :: IO ()
runTest = do
  case evaluateExpr of
    Just x -> putStrLn (getSyntax x)
    Nothing -> putStrLn "Failed"
  return ()
-}
