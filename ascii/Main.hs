{-# LANGUAGE TemplateHaskell, PackageImports, RecursiveDo #-}
module Main where

import Lib
import Control.Lens
import Data.Maybe
import Data.Functor
import Control.Applicative
import Data.List
import Data.Char (toUpper,chr)
import Control.Monad (forever,when,unless,join)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT,get,put,evalStateT)
import System.Exit (exitSuccess)
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin, stdout, hGetChar, hPutChar, fixIO )


clear = putStr "\ESC[2J"

keyToDir::Char->Dir
keyToDir c = case c of
              'h' -> MoveLeft
              'l' -> MoveRight
              'j' -> MoveDown
              'k' -> MoveUp
              _   -> NoMove

gameLoopState::(Int->IO GameField) -> StateT Game IO ()
gameLoopState levelReader = do 
              game <- get

              let field'      = game^.field
              let finishLevel = isWon field'

              -- disply
              liftIO clear
              liftIO $ putStrLn (show (game^.level) ++ show finishLevel)
              liftIO $ printGameField field'

              -- check level won
              when finishLevel 
                   (liftIO $ putStrLn $"WON Level " ++ show (game^.level)++" - press any key to continue")

              -- get input
              c<-liftIO getChar

              -- update 
              -- TODO put this into Game Model
              newGame <- if finishLevel then do
                                          nextLevel<-liftIO $ levelReader $ (game^.level) + 1
                                          return $ game & field .~ nextLevel 
                                                        & level +~ 1
                                        else 
                                          case c of
                                            'q' -> return game
                                            'r' -> do
                                                      repeatLevel<-liftIO $ levelReader (game^.level)
                                                      return $ game & field .~ repeatLevel
                                            _   -> return $ game & field .~ moveMan (keyToDir c) field'
                                                  
              put newGame

              -- check if quit, otherwise loop
              unless ( c == 'q') (gameLoopState levelReader)

main ::  IO b
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  field' <- fileLevelReader 0
  let initialGame = Game {_level=0, _field=field'}

  -- game loop
  evalStateT (gameLoopState fileLevelReader) initialGame

  exitSuccess
