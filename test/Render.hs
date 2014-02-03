{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
module Render where

import Control.Monad ( forM, when )

import Data.Either
import Data.Maybe

import Foreign.Ptr

import Data.IORef

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT

import System.Environment

data Viewer = Viewer
    {
        image_dirty :: Bool,
        mwindow_size :: Maybe Size,
        mrendering :: Maybe (IO ()),
        fbo_state :: FBOState,
        viewing_position :: (Int, Int),
        viewing_scale :: Double
    }

data FBOState = 
      NoFBO
    | HaveFBO FBOContext
    deriving (Eq, Show)

data FBOContext = FBOContext
    {
        fbo :: FramebufferObject,
        fbo_tex :: TextureObject,
        fbo_size :: Size
    }
    deriving (Eq, Show)

init_display = do
    name <- getProgName
    args <- getArgs
    args' <- initialize name args
    initialDisplayMode $= [RGBAMode, WithDepthBuffer, DoubleBuffered]
    createWindow "Render"
    return args'

new_viewer = do
    args <- init_display
    viewer_ref <- newIORef $ Viewer True Nothing Nothing NoFBO (0,0) 1.0
    displayCallback $= viewer_display viewer_ref
    reshapeCallback $= Just (reshape_viewer_window viewer_ref)
    keyboardMouseCallback $= Just (glut_event_handler viewer_ref)
    return (viewer_ref, args)

reshape_viewer_window :: IORef Viewer -> ReshapeCallback
reshape_viewer_window viewer_ref new_window_size = modifyIORef viewer_ref $ \viewer -> viewer { mwindow_size = Just new_window_size }

view_rendering viewer_ref r = modifyIORef viewer_ref $ \viewer -> viewer { image_dirty = True, mrendering = Just r }

initialize_fbo viewer_ref = do
    [fbo :: FramebufferObject] <- genObjectNames 1
    [fbo_tex :: TextureObject] <- genObjectNames 1
    bindFramebufferEXT FramebufferTarget fbo
    textureBinding Texture2D $= Just fbo_tex
    texImage2D Nothing NoProxy 0 RGBA8 (TextureSize2D 4096 4096) 0 (PixelData RGBA UnsignedByte nullPtr)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    framebufferTexture2DEXT FramebufferTarget (ColorAttachment 0) Texture2D fbo_tex 0
    textureBinding Texture2D $= Nothing
    let fbo_context = FBOContext fbo fbo_tex (Size 4096 4096)
    modifyIORef viewer_ref $ \viewer -> viewer { fbo_state = HaveFBO fbo_context }
    unbindFramebufferEXT FramebufferTarget

rasterize_rendering viewer_ref = do
    HaveFBO fbo_context <- readIORef viewer_ref >>= return . fbo_state
    bindFramebufferEXT FramebufferTarget $ fbo fbo_context
    preservingAttrib [ViewportAttributes] $ do
        viewport $= (Position 0 0, fbo_size fbo_context)
        fromJust . mrendering =<< readIORef viewer_ref
        finish
    unbindFramebufferEXT FramebufferTarget
    modifyIORef viewer_ref $ \viewer -> viewer { image_dirty = False }
    
viewer_display viewer_ref = do
    whenM (readIORef viewer_ref >>= return . isJust . mwindow_size) $ do
        whenM (readIORef viewer_ref >>= return . (== NoFBO) . fbo_state) $ initialize_fbo viewer_ref
        whenM ( do rerender <-  readIORef viewer_ref >>= return . image_dirty
                   can_render <- readIORef viewer_ref >>= return . isJust . mrendering
                   return $ can_render && rerender
              ) $ rasterize_rendering viewer_ref
        window_size <- return . fromJust . mwindow_size =<< readIORef viewer_ref
        HaveFBO fbo_context <- readIORef viewer_ref >>= return . fbo_state
        (pos_x, pos_y) <- readIORef viewer_ref >>= return . viewing_position
        s <- readIORef viewer_ref >>= return . viewing_scale
        viewport $= (Position 0 0, window_size)
        -- Establish the coordinate center (0,0) -> (1.0, 1.0)
        matrixMode $= Modelview 0
        loadIdentity
        scale s s 1
        translate $ Vector3 (-1.0 :: Double) (-1.0) 0.0
        scale (2.0 :: Double) 2.0 0.0
        -- 
        scale (1.0 / fromIntegral (width window_size)) 
              (1.0 / fromIntegral (height window_size)) 
              (1.0 :: Double)
        translate $ Vector3 (-1.0 * fromIntegral pos_x) (-1.0 * fromIntegral pos_y) (0.0 :: Double)
        -- Now render the FBO image.
        clearColor $= Color4 1.0 1.0 1.0 0.0
        clear [ColorBuffer]
        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just (fbo_tex fbo_context)
        textureFunction $= Replace
        polygonMode $= (Fill, Fill)
        let max_x :: Double = fromIntegral $ width $ fbo_size fbo_context
        let max_y :: Double = fromIntegral $ height $ fbo_size fbo_context
        renderPrimitive Quads $ do
            color $ Color3 (0.0 :: Double) 0.0 0.0
            vertex $ Vertex2 (0.0 :: Double) 0.0
            texCoord $ TexCoord2 (0.0 :: Double) 0.0
            vertex $ Vertex2 (0.0 :: Double) max_y
            texCoord $ TexCoord2 (0.0 :: Double) 1.0
            vertex $ Vertex2 max_x max_y
            texCoord $ TexCoord2 (1.0 :: Double) 1.0
            vertex $ Vertex2 max_x 0.0
            texCoord $ TexCoord2 (1.0 :: Double) 0.0
        textureBinding Texture2D $= Nothing
        finish
        swapBuffers
        return ()

glut_event_handler viewer_ref key key_state mods pos = do
    (pos_x, pos_y) <- readIORef viewer_ref >>= return . viewing_position
    let (pos_x', pos_y') = 
            case (key,key_state,mods,pos) of
                (SpecialKey KeyRight, Down, Modifiers Up Up Up, _) -> (pos_x + 10, pos_y)
                (SpecialKey KeyLeft, Down, Modifiers Up Up Up, _) -> (pos_x - 10, pos_y)
                (SpecialKey KeyUp, Down, Modifiers Up Up Up, _) -> (pos_x, pos_y + 10)
                (SpecialKey KeyDown, Down, Modifiers Up Up Up, _) -> (pos_x, pos_y - 10)
                otherwise -> (pos_x, pos_y)
    s <- readIORef viewer_ref >>= return . viewing_scale
    let s' = case (key,key_state,mods,pos) of
                (SpecialKey KeyUp, Down, Modifiers Down Up Up, _) -> s * 2
                (SpecialKey KeyDown, Down, Modifiers Down Up Up, _) -> s * 0.5
                otherwise -> s
    modifyIORef 
        viewer_ref 
        $ \viewer -> viewer 
            { 
                viewing_position = (pos_x', pos_y'),
                viewing_scale = s'
            }
    postRedisplay Nothing

whenM :: Monad m => m Bool -> m () -> m ()
whenM bm m = bm >>= (\b -> when b m)

width :: Size -> GLsizei
width (Size w _) = w

height :: Size -> GLsizei
height (Size _ h) = h

