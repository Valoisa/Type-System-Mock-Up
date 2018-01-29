module Helpers where

import Data.Maybe
import Control.Monad
import Data.Graph.Inductive.Query.Monad


processTypesList :: ([a], b) -> ((a, b) -> Maybe (a, b)) -> Maybe ([a], b)
processTypesList ([], c) _ = Just ([], c)
processTypesList ((x:xs), c) f = fxs >>= \y -> Just (fst (fromJust fx) : fst y, snd y)
    where
        fx  = Just (x, c) >>= f
        fxs = fx >>= \y -> processTypesList (xs, snd y) f

processType :: (a, b) -> ((a, b) -> Maybe (c, b)) -> Maybe (c, b)
processType x f = f x

processTypes :: ((a, a), b) -> ((a, b) -> Maybe (c, b)) -> Maybe ((c, c), b)
processTypes ((t1, t2), c) f = ft2 >>= \y -> Just ((fst (fromJust ft1) , fst y), snd y)
    where
        ft1 = Just (t1, c) >>= f
        ft2 = ft1 >>= \y -> processType (t2, snd y) f

maybeTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
maybeTuple (Just x, Just y) = Just (x, y)
maybeTuple _ = Nothing
