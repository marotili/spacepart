module Main where

import Verify.Data.SpacePart.QuadTree
import Verify.Data.SpacePart.AABB
import Verify.Data.SpacePart.Edge
import Test.QuickCheck

main = do
    quickCheck $ label "parallel_edges_do_not_intersect_prop" parallel_edges_do_not_intersect_prop 
    quickCheck $ label "intersects_is_reflexive_prop" intersects_is_reflexive_prop
    quickCheck $ label "encloses_is_reflexive_prop" encloses_is_reflexive_prop
    quickCheck $ label "element_bounds_query_is_element_prop" element_bounds_query_is_element_prop
    quickCheck $ label "oob_bounds_query_is_empty_prop" oob_bounds_query_is_empty_prop
    quickCheck $ label "all_elements_inserted_query_prop " all_elements_inserted_query_prop 
    putStrLn "DONE"

