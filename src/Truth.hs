{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Truth where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable (cata)
import Control.Monad (replicateM)
import Data.List (intersperse)
import Data.Foldable (fold)
import Data.Bool (bool)

data Pred a
  = Statement a
  | And (Pred a) (Pred a)
  | Or (Pred a) (Pred a)
  | Not (Pred a)
  | Implies (Pred a) (Pred a)
  | Equiv (Pred a) (Pred a)
  deriving (Functor, Foldable, Show)

makeBaseFunctor ''Pred

eval :: Ord a => Pred a -> Map a Bool -> Bool
eval p env = cata go p
  where
    go (StatementF t) = fromJust $ M.lookup t env
    go (AndF a b)     = a && b
    go (OrF a b)      = a || b
    go (NotF a)       = not a
    go (ImpliesF a b) = not a || b
    go (EquivF a b)   = a == b

pretty :: Pred Text -> Text
pretty = cata go
  where
    go (StatementF t) = t
    go (AndF a b)     = parens (a <> " & " <> b)
    go (OrF a b)      = parens (a <> " | " <> b)
    go (NotF a)       = "-" <> a
    go (ImpliesF a b) = parens (a <> " -> " <> b)
    go (EquivF a b)   = parens (a <> " <-> " <> b)

    parens x = "(" <> x <> ")"

statements :: Ord a => Pred a -> [a]
statements = S.toList . foldr S.insert S.empty

envs :: Ord a => Pred a -> [Map a Bool]
envs p =
  let ss = statements p
      bs = replicateM (length ss) [True, False]
  in M.fromList . zip ss <$> bs

tabulate :: Pred Text -> Text
tabulate p = header <> rows
  where
    header = fold (intersperse " | " (statements p)) <> " | " <> pretty p <> "\n"
    rows   = foldMap go $ envs p
      where
        go :: Map Text Bool -> Text
        go m =
          let vs = fold $ intersperse " | " $ fmap snd $ M.toList $ prettyBool <$> m
              r  = prettyBool $ eval p m
          in vs <> " | " <> r <> "\n"

prettyBool :: Bool -> Text
prettyBool = bool "F" "T"
