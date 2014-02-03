{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.QuadTree

import Data.AABB

import Render ( init_display
              , new_viewer
              , view_rendering
              )

import Control.Monad

import Data.Maybe ( maybe )

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT

import System.Random.Mersenne
import System.Random.Utils

main = do 
    (viewer, _) <- new_viewer
    gen <- newMTGen Nothing
    (rq, gen) <- random_quadtree gen (empty :: QuadTree Elem) 5
    view_quadtree viewer gen rq
    return ()

data Elem = Elem Boundary (Color3 Double)

instance HasBoundary Elem where
    boundary_points (Elem b _) = boundary_points b
    boundary_edges (Elem b _) = boundary_edges b
    boundary_extents (Elem b _) = boundary_extents b
    boundary_square (Elem b _) = boundary_square b

random_quadtree gen q 0 = return (q, gen)
random_quadtree gen q n = do
    x :: Double <- randomRange (-10.0) 10.0 gen
    y :: Double <- randomRange (-10.0) 10.0 gen
    s :: Double <- randomRange 0.001 1.0 gen
    let eb = Boundary (x, y) s
    r :: Double <- randomRange 0.0 1.0 gen
    g :: Double <- randomRange 0.0 1.0 gen
    b :: Double <- randomRange 0.0 1.0 gen
    let e = Elem eb (Color3 r g b)
    let q' = insert e q
    random_quadtree gen q' (n - 1)
    
view_quadtree viewer gen q = do
    view_rendering viewer $ display_quadtree gen q
    mainLoop

display_quadtree gen q@(QuadTree _ (Boundary (bx, by) bsize) _) = do
    clearColor $= Color4 1.0 1.0 1.0 0.0
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    -- First set the display to have the extents (0,0) and (1,1)
    translate $ Vector3 (-1.0 :: Double) (-1.0) 0.0
    scale (2.0 :: Double) 2.0 0.0
    -- Scale the entire quadtree to the display.
    scale (1.0 / bsize) (1.0 / bsize) 1.0
    translate $ Vector3 (-bx) (-by) 0.0
    lineWidth $= 2.0
    render_elements q
    outline_quadtree q
    flush

outline_quadtree q = do
    color $ Color3 (0.0 :: Float) 0.0 0.0
    polygonMode $= (Line, Line)
    renderPrimitive Quads $ outline_quadtree' q
    where 
        outline_quadtree' (QuadTree _ b (cq0, cq1, cq2, cq3)) = do
            mapM_ (maybe (return ()) $ \cq -> outline_quadtree' cq) [cq0, cq1, cq2, cq3]
            render_boundary b

render_elements q = do
    polygonMode $= (Fill, Fill)
    renderPrimitive Quads $ outline_elements' q
    where
        outline_elements' (QuadTree es _ (cq0, cq1, cq2, cq3)) = do
            mapM_ (maybe (return ()) $ \cq -> outline_elements' cq) [cq0, cq1, cq2, cq3]
            forM_ es $ \(Elem b c) -> do
                color c
                render_boundary b

random_color_gen gen = sequence $ repeat $ do
    r :: Double <- randomRange 0.0 1.0 gen
    g :: Double <- randomRange 0.0 1.0 gen
    b :: Double <- randomRange 0.0 1.0 gen
    return $ Color3 r g b

render_boundary (Boundary (x,y) size) = do
    vertex $ Vertex2 x y
    vertex $ Vertex2 (x + size) y
    vertex $ Vertex2 (x + size) (y + size)
    vertex $ Vertex2 x (y + size)

