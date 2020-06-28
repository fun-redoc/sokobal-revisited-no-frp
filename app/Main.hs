{-# LANGUAGE BlockArguments, TemplateHaskell #-}

module Main where

import Debug.Trace (trace)

import Lib
import Data.Either
import Data.Maybe (fromMaybe)
import Control.Monad
import Codec.BMP
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
--import Graphics.Gloss.Interface.Pure.Game
import System.IO
import System.Environment
import System.Exit
import Control.Lens
import Paths_sokoban_gloss

liftM'::Monad m=>(a->b)->(a->m b)
liftM' f x = return $ f x
liftM''::Monad m=>(a->b->c)->(a->b->m c)
liftM'' f x y = return $ f x y

type Size = Float
data AppMode = Ascii | Gloss deriving (Eq, Show, Read)
backgroundColor = makeColor 0 0 0 255
screenWidth  = 640::Float
screenHeight = 400::Float

handleInput::Event->GameState->GameState
--handleInput (EventKey _          Up   _ _) (StartGame game) = Playing game
handleInput (EventKey (Char c) Down _ _) (Playing game) = if isWon $ newGame^.field
                                                          then WonGame newGame
                                                          else Playing newGame
  where
    newGame = case c of
                   'h' -> moveManInGame MoveLeft  game
                   'j' -> moveManInGame MoveUp    game
                   'k' -> moveManInGame MoveDown  game
                   'l' -> moveManInGame MoveRight game
handleInput _                              gameState      = gameState

handleInputIO::Event->GameState->IO GameState
handleInputIO (EventKey _          Up   _ _) (StartGame game) = return $ Playing game
handleInputIO (EventKey _          Up   _ _) (WonGame   game) = undefined
handleInputIO (EventKey _          Up   _ _) (LostGame  game) = undefined
handleInputIO evt                            gameState        = return $ handleInput evt gameState

toPoint::Pos -> Point
toPoint = over both fromIntegral

mul::Point->Point->Point
mul (x,y) (x',y') = (x*x',y*y')
div::Point->Point->Point
div (x,y) (x',y') = (x/x', y/y')

data GameConfiguraton = GameConfiguraton { _objectSize::Size
                                         , _scaleFactors::Point
                                         , _wallTexture::Picture
                                         , _crateTexture::Picture
                                         , _cratePutTexture::Picture
                                         , _sokobanTexture::Picture
                                         }
makeLenses ''GameConfiguraton


gameAsPicture::GameConfiguraton->GameState->Picture
gameAsPicture gameConf (StartGame game) = uncurry scale (gameConf^.scaleFactors )
                                                        $ Color red
                                                        $ pictures [translate 0 (3*(gameConf^.objectSize)) $ Text "press some key"
                                                                   ,Text "to start game..."
                                                                   ]

gameAsPicture gameConf (Playing game) =
  pictures [ pictures $ fmap (translate_ (gameConf^.wallTexture) . toPoint) (game ^. (field . walls))
           , pictures $ fmap (translate_ (gameConf^.cratePutTexture) . toPoint) (game ^. (field . storage))
--           , pictures $ fmap (translate_ (Color (greyN 0.6) 
--                                          $ rectangleSolid (gameConf^.objectSize) (gameConf^.objectSize)
--                                         ) . toPoint) 
--                                         (game ^. (field . storage))
           , pictures $ fmap (translate_ (gameConf^.crateTexture) . toPoint) (game ^. (field . crates))
           , maybe Blank (translate_ (gameConf^.sokobanTexture) . toPoint) (game^.field.sokoban)
           ]
    where
    translate_ object (x,y) = translate (gameConf^.objectSize*x) (gameConf^.objectSize*y) object
gameAsPicture gameConf _ = undefined

loadPicture::FilePath->IO (Picture, Point)
loadPicture fileName = do
   bmp <- getDataFileName fileName >>= readBMP
   when (isLeft bmp) $ do print (bmp^?!_Left)
                          exitFailure
   let texture = bitmapOfBMP (bmp^?!_Right)
   let size = bitmapSize $ bitmapDataOfBMP (bmp^?!_Right)
   return (texture, toPoint size)


main :: IO ()
main = do
   -- load images
   (wallTexture, wallTextureSize) <- loadPicture "wall_50x50.bmp"
   let objectSize = uncurry max wallTextureSize
   --   crate
   (crateTexture, crateTextureSize) <- loadPicture "crate1_diffuse_50x50.bmp"
   --   create brought to its place
   (cratePutTexture, cratePutTextureSize) <- loadPicture "crate2_diffuse_50x50.bmp"
   --   sokoban
   (sokobanTexture', sokobanTextureSize) <- loadPicture "alien.bmp"
   let sokobanTexture =  scale (objectSize/(sokobanTextureSize^._1)) (objectSize/(sokobanTextureSize^._2)) sokobanTexture'
   print sokobanTextureSize

--   wallTexture <- trace "LOADING.." $ getDataFileName "wall_50x50.bmp" >>= loadBMP
--   foldr (\p acc -> max acc ((uncurry max) p)) minBound $ (field' ^.. walls . folded)
   args <- getArgs
   appMode <- case args of "ascii":[] -> return Ascii
                           "gloss":[] -> return Gloss
                           _          -> return Gloss
                           -- _ -> do putStrLn "Invalid arguments."
                           --          putStrLn "Usage:"
                           --          exitFailure
   field' <- fileLevelReader 200
   let initialGame = StartGame $ Game {_level=0, _field=field'}
   let (xmax, ymax) = smul objectSize
                    $ add (2,2)
                    $ toPoint
                    $ foldr (\(x,y) (x',y') -> (max x x', max y y')) (minBound, minBound) (field' ^.. walls . folded)
   let (scalex, scaley) = (screenWidth, screenHeight) `Main.div` (xmax, ymax)
   let scalePic = min scalex scaley
   print (scalex, scaley)

   let window = InWindow ("Sokoban "++show appMode) (round screenWidth, round screenHeight) (100, 100)
   playIO window backgroundColor 30 initialGame
               (liftM'  (translate (-(screenWidth/2)+(objectSize*scalePic))
                                   (-(screenHeight/2)+(objectSize*scalePic))
                                   . scale scalePic scalePic
                                   . gameAsPicture (GameConfiguraton objectSize
                                                                     (scalex, scaley)
                                                                     wallTexture
                                                                     crateTexture
                                                                     cratePutTexture
                                                                     sokobanTexture)
                                   ))
               --(liftM'' handleInput)
               handleInputIO
               (liftM'' (const id))
   exitSuccess
