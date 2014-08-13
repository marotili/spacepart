{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module Data.SpacePart.QuadTree where
import Data.SpacePart.AABB

import Debug.Trace
import Data.Maybe
import Data.List ( sortBy )
import qualified Data.List as List
import Data.VectorSpace

-- | A 2D binary hierarchical space subdivision of a region. 
-- All elements contained in the quadtree are required to have a Boundary. This is an axis aligned
-- box with congruent sides.
--
-- Each node of the quadtree is composed of:
-- 
-- 0. A list of elements who's shape can be queried for intersection with the quad.  These are all
-- the elements with a boundary that are fully enclosed by the boundary of this node but not fully
-- enclosed by a quadrant of this node. 
--
-- 1. The Boundary of this node.
--
-- 2. The child nodes of this node. Each is a quadrant of this nodes boundary.
--
data QuadTree e where
    QuadTree :: (HasBoundary e)
                => [e]
                -> Boundary
                -> ( Maybe (QuadTree e) , Maybe (QuadTree e)
                   , Maybe (QuadTree e) , Maybe (QuadTree e)
                   ) 
                -> QuadTree e

elements :: QuadTree e -> [e]
elements (QuadTree es _ _) = es

children :: QuadTree e -> ( Maybe (QuadTree e) , Maybe (QuadTree e)
                          , Maybe (QuadTree e) , Maybe (QuadTree e)
                          )
children (QuadTree _ _ c) = c

instance HasBoundary (QuadTree e) where
    boundary_edges (QuadTree _ bounds _) = boundary_edges bounds
    boundary_extents (QuadTree _ bounds _) = boundary_extents bounds
    boundary_square (QuadTree _ bounds _) = bounds

data Quadrant = 
        NPQuad | PPQuad
      | NNQuad | PNQuad
    deriving (Eq, Show)

{- An element of a quadtree can intersect the boundary of multiple nodes in the quadtree. This
 - only associates an element with a single node.  This permits the property: forall p. p <: paths
 - to a leaf node, forall e <: elements in the universe => p will enounter no more than one element
 - that references e.
 -
 - Which, I think, simplifies things. Maybe?
 - EG:
 -  let qt = QuadTree.empty
 -      qt' = QuadTree.insert qt e0
 -      qt'' = QuadTree.insert q' e1
 -  In the case where e1 entirely encompasses qt' there would be greater sharing betwee qt'' and qt'
 -  than if each node in the tree contained references to all elements that intersect that node.
 -
 -  On the other hand the query "All elements intersecting this child node of this quadtree." would
 -  require a full descent from the root to collect the list of elements. I could see this being a
 -  useful query. 
 -
 -  I think this is resolvable. The query necessitates a cursor like structure: The reference to a
 -  specific child node in a quadtree. Which could transparently cache the parent node element
 -  references.
 -} 

pp_quad (QuadTree _ _
         (  _, mq, 
            _,  _
         )
        ) 
    = mq
pn_quad (QuadTree _ _
         (  _,  _, 
            _, mq
         )
        ) 
    = mq
nn_quad (QuadTree _ _
         (  _,  _, 
           mq,  _
         )
        ) 
    = mq
np_quad (QuadTree _ _
         ( mq,  _, 
            _,  _
         )
        ) 
    = mq

map_child :: (Maybe (QuadTree e) -> Maybe (QuadTree e)) 
             -> Quadrant 
             -> (Maybe (QuadTree e), Maybe (QuadTree e)
                ,Maybe (QuadTree e), Maybe (QuadTree e)
                )
             -> (Maybe (QuadTree e), Maybe (QuadTree e)
                ,Maybe (QuadTree e), Maybe (QuadTree e)
                )
map_child f NPQuad ( np_c, pp_c
                   , nn_c, pn_c
                   ) = ( f np_c, pp_c
                       , nn_c  , pn_c
                       )
map_child f PPQuad ( np_c, pp_c
                   , nn_c, pn_c
                   ) = ( np_c, f pp_c
                       , nn_c, pn_c
                       )
map_child f NNQuad ( np_c, pp_c
                   , nn_c, pn_c
                   ) = ( np_c  , pp_c
                       , f nn_c, pn_c
                       )
map_child f PNQuad ( np_c, pp_c
                   , nn_c, pn_c
                   ) = ( np_c, pp_c
                       , nn_c, f pn_c
                       )

non_empty_children q =
    let (np_c, pp_c, nn_c, pn_c) = children q
    in catMaybes [np_c, pp_c, nn_c, pn_c]

{- | Returns an empty QuadTree without a specific boundary. The default bounds are centered around
 - (0,0) with a size of 2
 -
 - TODO: Alternatively an empty quadtree could have no defined bounds. The bounds would then be
 - defined on the first insertion. 
 -}
empty :: HasBoundary e => QuadTree e
empty = QuadTree [] (Boundary (-1,-1) 2) empty_children

{- | Returns an empty QuadTree with the given bounds.
 - The given bounds cannot have a size of 0. This will error out on that case.
 -
 - TODO: The user may find it easier for this to accept a 0 sized boundary which is transparently
 - changed to a non-0 sized boundary on insert.
 -}
empty_with_bounds :: HasBoundary e => Boundary -> QuadTree e
empty_with_bounds (Boundary _ 0.0) = error "Cannot construct a quadtree with 0 sized boundary."
empty_with_bounds bounds = QuadTree [] bounds empty_children

empty_children = ( Nothing, Nothing
                 , Nothing, Nothing
                 ) 

singleton_child NPQuad q = ( Just q , Nothing
                           , Nothing, Nothing
                           )
singleton_child PPQuad q = ( Nothing, Just q
                           , Nothing, Nothing
                           )
singleton_child NNQuad q = ( Nothing, Nothing
                           , Just q , Nothing
                           )
singleton_child PNQuad q = ( Nothing, Nothing
                           , Nothing, Just q
                           )

{-| Inserts the given element into the quadtree. 
 - This inserts the element into a this node or a child quadrant node if the current node encloses
 - the element.  Otherwise this inserts the element into a new node that is a parent of the given
 - node.
 -}
insert :: (HasBoundary e) => e -> QuadTree e -> QuadTree e
insert e q =
    if (boundary_square q) `encloses` (boundary_square e)
        then insert_self_or_child e q
        else insert_via_parent e q

{-| Inserts the given element into either a child node of the current node if one of the quadrants
 - encloses the element.
 - Otherwise the element is added to the current node's list of elements.
 -}
insert_self_or_child :: (HasBoundary e) => e -> QuadTree e -> QuadTree e
insert_self_or_child e q@(QuadTree es bounds quadrants) =
    case filter (\(cqb, _) -> cqb `encloses` (boundary_square e)) (quadrant_bounds q) of
        [child]      -> traceShow ("insert_child") $ insert_child child e q 
        _            -> traceShow ("insert_self") $ QuadTree (e : es) bounds quadrants

quadrant_bounds :: QuadTree e -> [(Boundary, Quadrant)]
quadrant_bounds (QuadTree _ (Boundary p size) _) = 
    let child_size = size / 2
        nn_p = p 
        np_p = p ^+^ (0         , child_size)
        pp_p = p ^+^ (child_size, child_size)
        pn_p = p ^+^ (child_size, 0         )
    in map (\(p, q) -> (Boundary p child_size, q))
        [ (nn_p, NNQuad)
        , (np_p, NPQuad)
        , (pp_p, PPQuad)
        , (pn_p, PNQuad)
        ]

{- insert_via_parent adds the given element to a new quadtree, q_e, that is connected to the given
 - quadtree, q, through a parent tree, q_root. 
 -
 - The two quadtrees q and q_e are both children on some path from q_root.
 -
 - There is at least one path from q_root to q and q_e. There may be multiple paths?
 -  let q = (-1, -1) -> 1
 -      q_e = (0,0) -> 1
 -      q_root = (-1,-1) -> 2
 - In the above case there is only one possible q_root with minimum bounds. However there are multiple
 - mays to connect q and q_e through a parent node.
 -      q_p_0 = (-2, -2) -> 2 [PP => q]
 -      q_p_1 = (0,0) -> 2 [NN => q_e]
 -      q_root = (-2, -2) -> 4 [NN => q_p_0, PP => q_p_1]
 -
 - I'm not really sure of how to optimally introduce a node for q_e and connect them through a
 - parent node.  There are incorrect methods. EG: Always picking the parent quadtree such that the
 - given quadtree is at a fixed position.  This could result in a search for a new encompasing
 - parent that never converges.
 -
 - The method used here is to add parent nodes to q until a parent node is found that encompass e.
 - This is a breadth first search of the generated graph
 - Nodes are parent quadtrees containing q as a child and encompasing e
 - Edges are directional (q_u, q_v). Each edge represents the operation of adding a parent to q_u
 - such that q_u is a specific quadrant of the parent.
 -
 - Given quadtree q and an element e:
 -  There is an edge from q for each of PNQuad, PPQuad, NPQuad, NNQuad to a parent quadtree with q
 -  as the given quadrant.
 -  This parent quadtree can be generated from q and the quadrant identifier.
 -}

-- | Adds the element to quadtree via a parent node to the given quadtree.
-- The parent to add e to is then the first of the possible parents nodes that enclose e.
insert_via_parent :: (HasBoundary e) 
                    => e
                    -> QuadTree e 
                    -> QuadTree e
insert_via_parent e q = 
    let q_root = first (\pq ->  (boundary_square pq) `encloses` (boundary_square e)) (parent_trees q)
    in insert_self_or_child e q_root
    where first f = fromJust . List.find f

-- | parent_trees generates all possible parent trees of the given tree (Without memoization) in the
-- order suitable for a breadth first search.
parent_trees q = parent_trees' [q]
    where 
        parent_trees' (q : qs) = 
            let parents = imm_parents q
            in parents ++ parent_trees' (qs ++ parents)
        imm_parents q_child = map (quadtree_with_child_in_quad q_child) [PNQuad, PPQuad, NPQuad, NNQuad]

quadtree_with_child_in_quad q@(QuadTree _ (Boundary (child_x,child_y) child_size) _) quad 
    | quad == NPQuad = QuadTree [] (Boundary (child_x, child_y - child_size) parent_size) $ singleton_child quad q
    | quad == PPQuad = QuadTree [] (Boundary (child_x - child_size, child_y - child_size) parent_size) $ singleton_child quad q
    | quad == PNQuad = QuadTree [] (Boundary (child_x - child_size, child_y) parent_size) $ singleton_child quad q
    | quad == NNQuad = QuadTree [] (Boundary (child_x, child_y) parent_size) $ singleton_child quad q
    where parent_size = child_size * 2

{- I wonder if there is a closed form solution to the search performed by insert_via_parent
 -
 -  For all Integer i =>
 -      The size of the quadrants at this level are equal to 
 -          size_i = base_size * 2^i
 -      For all Integer u,v => 
 -          The corner points of the quadrants are given by
 -          ( base_point.x + size_i * u, base_point.y + size_i * v)
 -  The search is for an (i,u,v) such that the quadrant identified by (i,u,v) completely encompases
 -  the element being inserted.
 -  For a given i it is possible to find a quadrant that either encompasses the element or
 -  intersects the elements boundary.
 -}

-- | Inserts the element in the child identified by the given boundary and Quadrant.
-- If there is no child at the given quadrant then a child is added and the element is inserted into
-- the new child.
insert_child :: (HasBoundary e) 
                => (Boundary, Quadrant) 
                -> e 
                -> QuadTree e
                -> QuadTree e
insert_child (cb, quad) e q@(QuadTree es b cs) = 
    let update_child = Just . insert_self_or_child e . maybe (QuadTree [] cb empty_children) id
    in QuadTree es b $ map_child update_child quad cs

{- | Returns all elements with boundaries that intersect the given boundary
 - By case:
 -  Boundary does not intersect quadtree
 -  Boundary intersects the quadtree
 -      All elements at the level of the quadtree could intersect the boundary. Test each element
 -      for intersection. 
 -      Descend into the quadrants
 -}
query :: (HasBoundary e) => Boundary -> QuadTree e -> [e]
query query_boundary = query' []
    where query' out q
            | not $ query_boundary `boundaries_intersect` (boundary_square q) = out
            | otherwise = 
                let es = filter (\e -> (boundary_square e) `boundaries_intersect` query_boundary) $ elements q
                in foldl (\out' cq -> query' out' cq) (out ++ es) (non_empty_children q)

