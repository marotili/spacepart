{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.SpacePart.AABB
    ( Boundary(..)
    , HasBoundary(..)
    , encloses
    , boundaries_intersect
    )
    where

import Data.VectorSpace
import Data.List (foldl')

type Vertex2 a = (a, a)
vec2 :: Double -> Double -> Vertex2 Double
vec2 x y = (x, y)

vx :: VectorSpace (v, v) => (v, v) -> v
vx (x, _) = x

vy :: VectorSpace (v, v) => (v, v) -> v
vy (_, y) = y

type Edge2 a = (Vertex2 a, Vertex2 a)
type LineSegment = Edge2 Double

-- | A 2D axis aligned square.
-- The boundary_corner defines the lower bound.
-- The boundary_size is the length of any edge of the square.
--
-- The boundary is inclusive on the low extent and exclusive on the max extent.
--
-- Used to represent both the 
-- 0. 2D axis aligned minimum bounding square of an element.
--
-- 1. The boundary of a quadtree element
--
data Boundary = Boundary
    {
        boundary_corner :: Vertex2 Double,
        boundary_size   :: Double
    }
    deriving (Eq, Show)

-- Boundaries b0 and b1 intersect if the min extent of the intersection of b1 with (the plane +x
-- including b0.p unioned with the plane +y including b0.p) is within b0.
boundaries_intersect :: (HasBoundary s0, HasBoundary s1) => s0 -> s1 -> Bool
boundaries_intersect s0 s1 =
    let b0 = boundary_square s0
        b1 = boundary_square s1
        c = (MinExtentPlanes $ boundary_corner b0)
    in if b1 `within_extents` c
        then let (Boundary p _) = region_within_extents b1 c
             in contains_point b0 p
        else False

newtype MinExtentPlanes = MinExtentPlanes (Vertex2 Double)
    deriving (Eq, Show)

-- A boundary intersects the min extent planes if the far extent of the boundary is within the range
-- defined by the min extent planes.  The comparison is > and not >= since the far extent is the
-- point just beyond the boundary. Which needs to be just inside the planes in order for the
-- boundary to be inside the planes.
within_extents b (MinExtentPlanes (min_x, min_y)) =
    let ((b_min_x, b_min_y), (b_max_x, b_max_y)) = boundary_extents b
    in if b_min_x == min_x && b_min_y == min_y
        then True
        else (b_max_x > min_x) && (b_max_y > min_y)

region_within_extents :: Boundary -> MinExtentPlanes -> Boundary
region_within_extents (Boundary p size) (MinExtentPlanes min_p) = Boundary (ext_max min_p p) size

{-
instance Intersectable Boundary LineSegment where
   intersects b l@(p0, p1) =
-- If any point of the line segment is contained in the boundary then the line segment intersects the
-- element.
       intersects b p0 || intersects b p1
-- If niether point is in the element the line segment could still intersect the boundary. The line
-- segment must, in this case, intersect an edge of the boundary.
       || any (intersects l) (boundary_edges b)

-}

union_boundaries :: Boundary -> Boundary -> Boundary
union_boundaries b0 b1 =
    let (min0, max0) = boundary_extents b0
        (min1, max1) = boundary_extents b1
        p = ext_min min0 min1
        ext = ext_max max0 max1
        (w,h) = ext ^-^ p
        size = max w h
    in Boundary p size

ext_min (x0,y0) (x1,y1) = (min x0 x1, min y0 y1)
ext_max (x0,y0) (x1,y1) = (max x0 x1, max y0 y1)

contains_point bounds (px, py) =
    let (x, y) = boundary_corner bounds
        s = boundary_size bounds
    -- If the point is equal to the corner point then consider it intersecting.
    -- The inclusive nature of the min extent "wins out" over the exclusive nature of the max
    -- extent.
    in if x == px && y == py
        then True
        else px < (x + s) && px >= x && py < (y + s) && py >= y

{- | A instance of HasBoundary has an axis aligned boundign square defined that entirely encloses
 - the space represented by the type.
 -}
class HasBoundary s where
    boundary_edges :: s -> [Edge2 Double]
    boundary_edges s = 
        let ps@(p0 : ps') = boundary_points s
        in zip ps (ps' ++ [p0])
    boundary_extents :: s -> (Vertex2 Double, Vertex2 Double)
    boundary_extents s =
        let (p0 : ps) = boundary_points s
            initial_min_extent = p0
            initial_max_extent = p0
            union_extents ((min_x, min_y), (max_x,max_y)) (x, y) =
                let min_x' = min min_x x
                    min_y' = min min_y y
                    max_x' = max max_x x
                    max_y' = max max_y y
                in ((min_x', min_y'), (max_x', max_y'))
        in foldl' union_extents (initial_min_extent, initial_max_extent) ps
    boundary_square :: s -> Boundary
    boundary_square s =
        let (min_extent, max_extent) = boundary_extents s
            width  = fst max_extent - fst min_extent
            height = snd max_extent - snd min_extent
            size = max width height
        in Boundary (fst min_extent, snd min_extent) size

-- A boundary cleary has itself as it's boundary.
instance HasBoundary Boundary where
    boundary_extents (Boundary p s) = (p, p ^+^ (s,s))
    boundary_square b = b

boundary_points :: HasBoundary s => s -> [Vertex2 Double]
boundary_points s = boundary_points' $ boundary_square s
    where 
        boundary_points' (Boundary p s) = 
            [ p
            , p ^+^ (0, s)
            , p ^+^ (s, s)
            , p ^+^ (s, 0)
            ]

{-| Returns true if the first boundary entirely encloses the second boundary.
 - This is expected to be reflexive.
 -}
encloses :: Boundary -> Boundary -> Bool
encloses (Boundary (x0,y0) s0) (Boundary (x1,y1) s1) = (x0 <= x1 && x0 + s0 >= x1 + s1) && (y0 <= y1 && y0 + s0 >= y1 + s1)

