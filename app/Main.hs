{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns #-}

module Main where

import Debug.Trace (trace)

import Lib
import qualified Data.Vector as V
import Data.Either
import Data.Maybe (fromMaybe)
import Data.List ( intercalate )
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Codec.BMP
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
--import Graphics.Gloss.Interface.Pure.Game
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Error
import System.Environment
import System.Directory (getHomeDirectory)
import System.Exit
import Control.Lens
import qualified Sound.ALUT as AL hiding (Static)
import Paths_sokoban_gloss

level_path::String
level_path = "/.sokobanLastLevel.txt"

loadSound path = do
      -- Create an AL buffer from the given sound file.
      buf <- AL.createBuffer  (AL.File path)
      source <- AL.genObjectName
      AL.buffer source AL.$= Just buf
      return source

playSound :: AL.Source -> IO ()
playSound source = do
    AL.play [source]
    -- Normally nothing should go wrong above, but one never knows...
    errs <- AL.get AL.alErrors
    unless (null errs) $
        hPutStrLn stderr (intercalate "," [ d | AL.ALError _ d <- errs ])
    return ()

playLoop :: AL.Source -> IO ()
playLoop source = do
    AL.loopingMode source AL.$= AL.Looping
    playSound source

liftM'::Monad m=>(a->b)->(a->m b)
liftM' f x = return $ f x
liftM''::Monad m=>(a->b->c)->(a->b->m c)
liftM'' f x y = return $ f x y

type Size = Float
data AppMode = Ascii | Gloss deriving (Eq, Show, Read)
backgroundColor = makeColor 0 0 0 255
screenWidth  = 640::Float
screenHeight = 400::Float


getLastLevel::IO Int
getLastLevel = do
    res <- tryIOError $ do
                          homeDir <- getHomeDirectory
                          handle <- openFile (homeDir++level_path) ReadMode
                          !contents <- hGetContents handle
                          hClose handle
                          return (read contents::Int)
    return $ fromRight 0 res

-- TODO should not be used in mai thread
putLevel::Int->IO ()
putLevel n = do
    res <- tryIOError $ do
                          homeDir <- getHomeDirectory
                          handle <- openFile (homeDir++level_path) WriteMode
                          hPutStr handle (show n)
                          hClose handle
    return ()

-- TODO should not be used in mai thread
loadLevel::Int->IO GameState
loadLevel n = do level' <- fileLevelReader n
                 return $ case level' of
                            Nothing -> ErrorState "Failed to load Level."
                            Just level -> Playing $ Game n level []

restartLevel game = loadLevel (game^.level)

handleInput::Event->GameState->GameState
handleInput (EventKey (Char c) Down _ _) (Playing game) = if isWon $ newGame^.field
                                                          then WonGame newGame
                                                          else Playing newGame
  where
    newGame = case c of
                   'h' -> moveManInGame MoveLeft  game
                   'j' -> moveManInGame MoveUp    game
                   'k' -> moveManInGame MoveDown  game
                   'l' -> moveManInGame MoveRight game
                   'a' -> moveManInGame MoveLeft  game
                   's' -> moveManInGame MoveUp    game
                   'w' -> moveManInGame MoveDown  game
                   'd' -> moveManInGame MoveRight game
                   _   -> game
handleInput _                              gameState      = gameState

handleInputIO::Event->GameState->IO GameState
handleInputIO (EventKey (Char 'q') Down _ _) (Playing game)        = do putLevel (game^.level)
                                                                        exitSuccess
handleInputIO (EventKey (Char 'r') Down _ _) (Playing game)        = restartLevel game
handleInputIO (EventKey (Char 'u') Down _ _) (Playing game)        = return $ Playing $ if null (game^.history)
                                                                                         then game
                                                                                         else head (game^.history)
handleInputIO (EventKey (Char 'q') Down _ _) _                     = exitSuccess
handleInputIO (EventKey _          Up   _ _) StartGame        = do savedLevel <- getLastLevel
                                                                   loadLevel savedLevel
handleInputIO (EventKey (SpecialKey KeyEnter) Up _ _) (WonGame   game) = do putLevel (game^.level)
                                                                            loadLevel (game^.level + 1)

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
gameAsPicture gameConf (ErrorState desc) = uncurry scale (gameConf^.scaleFactors)
                                                           $ Color red
                                                           $ pictures [ translate 0 (6*(gameConf^.objectSize)) $ Text desc
                                                                      , translate 0 (3*(gameConf^.objectSize)) $ Text "press some key"
                                                                      , Text "to quit..."
                                                                      ]
gameAsPicture gameConf StartGame = uncurry scale (gameConf^.scaleFactors )
                                                        $ Color yellow
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
gameAsPicture gameConf (WonGame game) = uncurry scale (gameConf^.scaleFactors)
                                                           $ Color green
                                                           $ pictures [ translate 0 (6*(gameConf^.objectSize)) $ Text ("you've won level " ++ show (game^.level + 1))
                                                                      , translate 0 (3*(gameConf^.objectSize)) $ Text "press some key"
                                                                      , Text "to enter next level..."
                                                                      ]
gameAsPicture gameConf _ = undefined

loadPicture::FilePath->IO (Picture, Point)
loadPicture fileName = do
   bmp <- getDataFileName fileName >>= readBMP
   when (isLeft bmp) $ do print (bmp^?!_Left)
                          exitFailure
   let texture = bitmapOfBMP (bmp^?!_Right)
   let size = bitmapSize $ bitmapDataOfBMP (bmp^?!_Right)
   return (texture, toPoint size)


gameLoopIO::Float->GameState->IO GameState
--gameLoopIO dt (Moving game) = return $ Moving game
gameLoopIO dt = return

main :: IO ()
--main = do
main = AL.withProgNameAndArgs AL.runALUT $ \progName args -> do
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
   -- sounds
--   thumpSound <- loadSound "thump.wav"
   --getDataFileName "BluesLoops_11_StayOnBeat.com.wav" >>= print
   bluesLoop <- getDataFileName "BluesLoops_11_StayOnBeat.com.wav" >>= loadSound
   AL.sourceGain bluesLoop AL.$= 0.1 -- lower the volume of the loop
   playLoop bluesLoop

   args <- getArgs
   appMode <- case args of "ascii":[] -> return Ascii
                           "gloss":[] -> return Gloss
                           _          -> return Gloss
                           -- _ -> do putStrLn "Invalid arguments."
                           --          putStrLn "Usage:"
                           --          exitFailure
   --field' <- fileLevelReader 200
   --let initialGame = StartGame $ Game {_level=0, _field=field'}
   let initialGame = StartGame -- $ Game {_level=0, _field=field'}
   allLevels <- allLevelsFileLevelReader
   let (xmax, ymax) = smul objectSize
                    $ add (2,2)
                    $ toPoint
                    $ foldr (\(x,y) (x',y') -> (max x x', max y y')) (minBound, minBound) (V.foldr (\f a->a++(f^.walls)) [] allLevels)

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
               --(liftM'' (const id))
               handleInputIO
               gameLoopIO
   exitSuccess
