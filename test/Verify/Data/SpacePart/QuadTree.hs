{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Verify.Data.SpacePart.QuadTree ( module Data.SpacePart.QuadTree
                                      , module Verify.Data.SpacePart.QuadTree
                                      )
    where

import Data.SpacePart.QuadTree

import Verify.Data.SpacePart.AABB

import Control.Monad
import Data.List hiding (insert)
import Test.QuickCheck

data BoundaryQTConstruction = BoundaryQTConstruction [Boundary] (QuadTree Boundary)
    deriving (Show)

instance Arbitrary BoundaryQTConstruction where
    arbitrary = do
        element_count <- choose (1,100)
        (q, es) <- foldM (\(q, es) _ -> do
                           e <- arbitrary
                           return (insert e q, e : es)
                         ) 
                         (empty, []) 
                         [1 :: Int .. element_count]
        return $ BoundaryQTConstruction es q

-- All elements inserted into the quadtree should be returned by a query for all elements within the
-- boundaries of the quadtree
all_elements_inserted_query_prop :: BoundaryQTConstruction -> Bool
all_elements_inserted_query_prop (BoundaryQTConstruction es q) = es \\ query (boundary_square q) q == []

element_bounds_query_is_element_prop :: Boundary -> Boundary -> Property
element_bounds_query_is_element_prop initial_bounds element_bounds = 
    boundary_size initial_bounds /= 0.0 ==>
    let q = empty_with_bounds initial_bounds
        q' = insert element_bounds q
    in case query element_bounds q' of
        []  -> False
        [e] -> e == element_bounds
        _   -> False

oob_bounds_query_is_empty_prop :: NonIntersectingBounds -> Property
oob_bounds_query_is_empty_prop (NonIntersectingBounds b_0 b_1) = 
    boundary_size b_0 /= 0.0 ==>
    let q :: QuadTree Boundary = empty_with_bounds b_0
    in  [] ==  query b_1 q 

-- An easy quadtree to test is one where the elements contained in the quadtree are boundaries.
instance Show (QuadTree Boundary) where
    show (QuadTree es b cq) = show es ++ " " ++ show b ++ " " ++ show cq ++ "\n"

{-
instance Show ( Maybe (QuadTree Boundary), Maybe (QuadTree Boundary)
              , Maybe (QuadTree Boundary), Maybe (QuadTree Boundary) ) where
    show (mq0, mq1, mq2, mq3) = "( " ++ show (fmap show mq0) ++ "," ++ show (fmap show mq1) ++
                                "," ++ show (fmap show mq2) ++ "," ++ show (fmap show mq3) ++ ")"

-}

