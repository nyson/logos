{-# LANGUAGE LambdaCase #-}

module Logos.Syntax where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader
import Data.List (subsequences, intercalate)
import Data.Maybe (isJust, fromJust)

-- Environment
type Env = Reader (Map String Bool)

data Expr
  = And Expr Expr
  | Or Expr Expr
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

  And  e1 e2 -> and <$> mapM eval [e1, e2]
  Or   e1 e2 -> or  <$> mapM eval [e1, e2]
  Impl e1 e2 -> eval $ Or e2 $ Neg e2

runEval :: [(String, Bool)] -> Expr -> Bool
runEval env = flip runReader (Map.fromList env) . eval

find :: String -> Env Bool
find var = Map.lookup var <$> ask >>= \case
  Just v -> return v
  Nothing -> error $ concat [var, " is not defined!"]

searchVars :: Expr -> [String]
searchVars = \case
  And e1 e2 -> searchVars e1 ++ searchVars e2
  Or e1 e2 -> searchVars e1 ++ searchVars e2
  Impl e1 e2 -> searchVars e1 ++ searchVars e2
  Neg e -> searchVars e
  T -> []
  F -> []
  Var str -> return str

truthTable :: [[(String, Bool)]] -> IO ()
truthTable tt = do
  let title = intercalate " | " $ map fst $ head tt
      boolChr t = if t then "T" else "F"
      truthValues = map (intercalate " | " . map (boolChr . snd)) tt
  putStrLn title
  mapM_ putStrLn truthValues

solveSystem :: System -> [[(String, Bool)]]
solveSystem (System pred hypothesis)
  = map fromJust
    $ filter isJust
    $ flip map table
    $ \env -> case and $ map (runEval env) (hypothesis:pred) of
                True -> Just env
                False -> Nothing
  where vars = concatMap searchVars pred
        table = possibilities vars

possibilities :: [String] -> [[(String, Bool)]]
possibilities strs = map truthValues $ subsequences strs
  where truthValues ss = map (\s -> (s, s `elem` ss)) strs

ex = (Var "x" `Impl` Var "y")

test = do
  let tests = [
        ("a => b", Var "a" `Impl` Var "b"),
        ("a && b", Var "a" `And` Var "b"),
        ("a || b", Var "a" `Or` Var "b")
        ]

  mapM_ (\(t, l) -> mapM_ putStrLn ["", t, "_______"] >> truthTable (solveSystem $ System [l] T)) tests
