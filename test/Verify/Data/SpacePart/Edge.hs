module Verify.Data.SpacePart.Edge where
import Data.SpacePart.Edge

import Data.AffineSpace

import Test.QuickCheck

data ParallelEdges = ParallelEdges Edge2D Edge2D
    deriving (Eq, Show)

instance Arbitrary ParallelEdges where
    arbitrary = do
        e0p0 <- arbitrary
        e0p1 <- arbitrary
        dx <- choose (0.01, 1000.0)
        dy <- choose (0.01, 1000.0)
        let d = (dx, dy)
        let e1p0 = e0p0 .+^ d
            e1p1 = e0p1 .+^ d
        return $ ParallelEdges (e0p0, e0p1) (e1p0, e1p1)

parallel_edges_do_not_intersect_prop :: ParallelEdges -> Bool
parallel_edges_do_not_intersect_prop (ParallelEdges e0 e1) = not $ edges_intersect e0 e1
