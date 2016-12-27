{-# LANGUAGE LambdaCase, GADTs #-}

module Logos.Syntax where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader
import Data.List (subsequences, intercalate, sort)
import Data.Maybe (isJust, fromJust)

data TruthTable = TT [String] [[Bool]]

instance Show TruthTable where
  show tt@(TT vars values) | validTT tt
    = concat [roof title,
              '\n':title,
              divider title,
              body,
              bottom title]
    where roof str = concat ["\n┌", replicate (length str - 2) '─', "┐"]
          title = concat ["│ ", intercalate " │ " vars, " │"]
          divider str = concat [
            "\n│", map (\x -> if x == '│' then '+' else '-') . init . tail $ str,
             "│"]
          body  = concatMap (wrap . intercalate " │ " . map boolChr) values
          wrap str = concat ["\n│ ", str, " │"]
          bottom str = concat ["\n└", replicate (length str - 2) '─', "┘"]
          boolChr True  = "T"
          boolChr False = "F"
  show _  = error "Not a valid truth table!"

validTT (TT vars vals) = all ((== vl) . length) vals
  where vl = length vars

-- Environment
type Env = Reader (Map String Bool)

data Expr
  = And Expr Expr
  | NAnd Expr Expr
  | Or Expr Expr
  | NOr Expr Expr
  | Impl Expr Expr
  | Neg Expr
  | T
  | F
  | Var String
  deriving (Show, Eq)

data System = System [Expr] Expr
  deriving (Show, Eq)

eval :: Expr -> Env Bool
eval = \case
  T -> return True
  F -> return False
  Var str -> find str

  Neg ex -> not <$> eval ex

  NAnd e1 e2 -> not . and <$> mapM eval [e1, e2]
  NOr  e1 e2 -> not . or  <$> mapM eval [e1, e2]

  And  e1 e2 -> and <$> mapM eval [e1, e2]
  Or   e1 e2 -> or  <$> mapM eval [e1, e2]
  Impl e1 e2 -> do
    [a, b] <- mapM eval [e1, e2]
    return $ not a || b

runEval :: [(String, Bool)] -> Expr -> Bool
runEval env = flip runReader (Map.fromList env) . eval

find :: String -> Env Bool
find var = Map.lookup var <$> ask >>= \case
  Just v -> return v
  Nothing -> error $ var ++ " is not defined!"

searchVars :: Expr -> [String]
searchVars = uniq . search
  where search = \case
          NAnd e1 e2 -> search e1 ++ search e2
          And e1 e2 -> search e1 ++ search e2
          NOr e1 e2 -> search e1 ++ search e2
          Or e1 e2 -> search e1 ++ search e2
          Impl e1 e2 -> search e1 ++ search e2
          Neg e -> search e
          T -> []
          F -> []
          Var str -> return str
        uniq = uniq' . sort
        uniq' = \case
          (x:y:xs) | x == y    -> uniq' (y:xs)
                   | otherwise -> x: uniq'(y:xs)
          xs -> xs

truthTable :: [[(String, Bool)]] -> TruthTable
truthTable tt = TT vars values
  where vars = map fst $ head tt
        values = map (map snd) tt

solveSystem :: System -> [[(String, Bool)]]
solveSystem (System pred hypothesis)
  = map fromJust
    $ filter isJust
    $ flip map table
    $ \env -> if all (runEval env) (hypothesis:pred)
              then Just env
              else Nothing
  where vars = concatMap searchVars pred
        table = possibilities vars

possibilities :: [String] -> [[(String, Bool)]]
possibilities strs = map truthValues $ subsequences strs
  where truthValues ss = map (\s -> (s, s `elem` ss)) strs

ex = (Var "a" `Impl` Var "b") `NAnd` (Neg (Var "a") `Or` Var "b")

test = mapM_ printTest [
  ("a => b", Var "a" `Impl` Var "b"),
  ("a && b", Var "a" `And` Var "b"),
  ("a || b", Var "a" `Or` Var "b"),
  ("!a || b", (Neg $ Var "a") `Or` Var "b")
  ]
  where printTest (t, l) = do
          putStrLn $ "\n" ++ t
          print $ truthTable (solveSystem $ System [l] T)

