{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.SpacePart.Edge
    where 

import Data.AffineSpace
import Data.VectorSpace

type Edge a = (a,a)

type P2D = (Double, Double)

--instance (VectorSpace s) => AffineSpace ((,) s s) where
--    type Diff (s, s) = (s, s)
--    p0 .-. p1 = p0 ^-^ p1
--    p .+^ v = p ^+^ v

type Edge2D = Edge P2D

-- | The equations for edge intersection are pulled from 
--  http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
-- without much thought.
edges_intersect (p0a, p0b) (p1a, p1b) = 
   let x1 = fst p0a
       y1 = snd p0a
       x2 = fst p0b
       y2 = snd p0b
       x3 = fst p1a
       y3 = snd p1a
       x4 = fst p1b
       y4 = snd p1b
       div = (y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1)
   in if div < 1e-9 
       then False
       else
       let t0n = (x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)
           t0 = t0n / div
           t1n = (x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)
           t1 = t1n / div
       in t0 > 0.0 && t0 < 1.0 && t1 > 0.0 && t1 < 1.0

