{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module NestedList where

import GHC.Exts
import Data.List

data NestedList a = List [NestedList a] | Item a
    deriving (Read, Eq)

instance (Show a) => Show (NestedList a) where
    show (Item v) = show v ++ ","
    show (List v) = "[" ++ concat [show x | x <- v] ++ "]"

instance (Num a) => Num (NestedList a) where
    fromInteger = Item . fromInteger

instance IsList (NestedList a) where 
    type Item (NestedList a) = NestedList a

    fromList :: [NestedList a] -> NestedList a
    fromList = List
