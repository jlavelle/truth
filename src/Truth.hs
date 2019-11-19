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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Foldable (fold)
import Data.Bool (bool)
import Data.Functor (($>))

data Pred a
  = Statement a
  | And (Pred a) (Pred a)
  | Or (Pred a) (Pred a)
  | Not (Pred a)
  | Implies (Pred a) (Pred a)
  | Equiv (Pred a) (Pred a)
  deriving (Functor, Foldable, Show)

makeBaseFunctor ''Pred

data Argument a = Argument
  { argPremises   :: NonEmpty (Pred a)
  , argConclusion :: Pred a
  }

toPred :: Argument a -> Pred a
toPred (Argument (p :| ps) c) = Implies (foldr And p ps) c

eval :: Ord a => Pred a -> Map a Bool -> Bool
eval p env = cata go p
  where
    go (StatementF t) = fromJust $ M.lookup t env
    go (AndF a b)     = a && b
    go (OrF a b)      = a || b
    go (NotF a)       = not a
    go (ImpliesF a b) = not a || b
    go (EquivF a b)   = a == b

valid :: Ord a => Argument a -> Bool
valid a@(Argument (p :| ps) _) =
  let pr = toPred a
      vs = NE.filter (eval (foldr And p ps)) $ envs pr
  in all id $ fmap (eval pr) vs

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

statements :: Ord a => Pred a -> NonEmpty a
statements = NE.fromList . S.toList . foldr S.insert S.empty

envs :: Ord a => Pred a -> NonEmpty (Map a Bool)
envs p =
  let ss = statements p
      bs = NE.fromList $ replicateM (length ss) [True, False]
  in mapFromFoldable . NE.zip ss . NE.fromList <$> bs

trueEnv :: Ord a => Pred a -> Map a Bool
trueEnv p =
  let s = statements p
  in mapFromFoldable $ NE.zip s (s $> True)

tabulate :: Pred Text -> Text
tabulate p = header <> rows
  where
    header = fold (NE.intersperse " | " (statements p)) <> " | " <> pretty p <> "\n"
    rows   = foldMap go $ envs p
      where
        go :: Map Text Bool -> Text
        go m =
          let vs = fold $ intersperse " | " $ fmap snd $ M.toList $ prettyBool <$> m
              r  = prettyBool $ eval p m
          in vs <> " | " <> r <> "\n"

prettyBool :: Bool -> Text
prettyBool = bool "F" "T"

mapFromFoldable :: (Ord k, Foldable f) => f (k, v) -> Map k v
mapFromFoldable = foldr (\(k, v) -> M.insert k v) M.empty
