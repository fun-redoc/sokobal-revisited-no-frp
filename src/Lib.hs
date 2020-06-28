{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Data.Maybe (isNothing)
import Data.List
import Control.Lens
import Paths_sokoban_gloss
import qualified Data.Vector as V

type Field2d a = [[a]]
type Pos = (Int,Int)
type DirVec = (Int,Int)


someFunc::IO ()
someFunc = putStrLn "NOT IMPLEMENTED"

stop, goRight, goLeft, goUp, goDown::DirVec
stop    = (0,0)
goRight = (1,0)
goLeft  = (-1,0)
goUp    = (0,-1)
goDown  = (0,1)

allIn::(Eq a)=>[a]->[a]->Bool
-- allIn [] [] = True
allIn [] _ = True
-- allIn (v:[]) (x:xs) = (v `elem` xs)
allIn (v:vs) xs = (v `elem` xs) && (vs `allIn` xs)

add :: (Num t, Num t1) => (t, t1) -> (t, t1) -> (t, t1)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

smul :: (Num t) => t -> (t, t) -> (t, t)
smul n (x2,y2) = (n*x2,n*y2)


-- see: https://de.wikipedia.org/wiki/Sokoban
testLevelChars:: Field2d Char
testLevelChars =             ["#######",
                              "## * @#",
                              "#*. o #",
                              "#######"]

data GameField = GameField { _walls::[Pos], _storage::[Pos], _crates::[Pos], _sokoban::Maybe Pos } deriving (Eq, Show)
makeLenses ''GameField

data Game = Game { _level::Int, _field::GameField}
makeLenses ''Game

data GameState = StartGame --      Game
               | Playing           Game
               | WonGame           Game
               | LostGame          Game
               | FinishedAllLevels Game
               | ErrorState        String
makeLenses ''GameState

data Dir = NoMove | MoveRight | MoveLeft | MoveUp | MoveDown deriving (Show, Eq)

moveMultipleCrates :: (Int, Int) -> (Int, Int) -> GameField -> (Bool, [Pos])
moveMultipleCrates (sx,sy) (dx,dy) gf = moveTransitive (sx,sy) (dx,dy) (_crates gf) gf

moveTransitive :: (Int, Int) -> (Int, Int) -> [Pos] -> GameField -> (Bool, [Pos])
moveTransitive pos dir ps gf = if null adjacentAndTransitive
                               then (numAdjacent == 0, ps)
                               else (True, map (\p->if p `elem` adjacentAndTransitive then p `add` dir else p) ps)
  where
  (numAdjacent, adjacentAndTransitive) = findAdjacentAndTransitive' (0,[])
    where
    findAdjacentAndTransitive' (n, res)
      | isWall gf pos' = (n,[])
      | isNothing idx  = (n,res)
      | otherwise      = findAdjacentAndTransitive' (n+1, pos':res)
      where
          pos' = pos `add` (n `smul` dir)
          idx  = pos' `elemIndex` ps

moveManInGame::Dir->Game->Game
moveManInGame dir game = game & field .~ moveMan dir (game^.field)

moveMan::Dir->GameField->GameField
moveMan NoMove    = id
moveMan MoveRight = moveMan' goRight
moveMan MoveLeft  = moveMan' goLeft
moveMan MoveUp    = moveMan' goUp
moveMan MoveDown  = moveMan' goDown

moveMan'::DirVec->GameField->GameField
moveMan' (dx,dy) gf = if moveSuccess
                      then gf & sokoban .~ manMovedPos
                              & crates .~ cratesMoved
                      else gf
  where
  manMovedPos = (\oldPos dir->let newPos = add oldPos dir
                              in if isWall gf newPos then oldPos else newPos
                )
                <$> gf^.sokoban
                <*> pure (dx,dy)
  (moveSuccess,cratesMoved)              = maybeMoveMultipeCrates manMovedPos
  maybeMoveMultipeCrates Nothing         = (False, [])
  maybeMoveMultipeCrates (Just startPos) = moveMultipleCrates startPos (dx,dy) gf

initGameField::GameField
initGameField = GameField [] [] [] Nothing

isStorage::GameField->Pos->Bool
isStorage gf pos = pos `elem` gf^.storage

isWall::GameField->Pos->Bool
isWall gf pos = pos `elem` gf^.walls

isWon :: GameField -> Bool
isWon gf = (gf^.crates) `allIn` (gf^.storage)

splitOnEmptyString::[String]->[[String]]
splitOnEmptyString xs = splitOnEmptyString' xs [] []
  where
    splitOnEmptyString'::[String]->[String]->[[String]]->[[String]]
    splitOnEmptyString' [] a b = b++[a]
    splitOnEmptyString' (c:cs) a b
      | c == ""   = splitOnEmptyString' cs []       (if null a then b else b++[a])
      | otherwise = splitOnEmptyString' cs (a++[c]) b

charsFromGameField:: GameField -> Field2d Char
charsFromGameField gf = manLayer (_sokoban gf)
  where
    manLayer Nothing                  = crateLayer
    manLayer (Just pos)               = updateField (if isStorage gf pos then '+' else '@') pos crateLayer
    crateLayer                        = foldl (\field (x,y)->updateField (if isStorage gf (x,y) then '*' else 'o') (x,y) field) storageLayer (gf^.crates)
    storageLayer                      = foldl (\field (x,y)->updateField '.' (x,y) field) wallLayer (gf^.storage)
    wallLayer                         = foldl (\field (x,y)->updateField '#' (x,y) field) (emptyField ' ') (gf^.walls)
    emptyField c                      = replicate height $ replicate width c
    (width, height)                   = (\(a,b)->(a+1,b+1)) $  foldl (\(ax,ay) (x,y)->(max ax x,max ay y)) (0,0) (gf^.walls)
    updateField::a->Pos->[[a]]->[[a]]
    updateField val (x,y) fld         = updateList (updateList val x (fld!!y)) y fld
    updateList val i ls               = take i ls ++[val]++ drop (i+1) ls

gameFieldFromChars::Int->Field2d Char->GameField
gameFieldFromChars i css = fromRows (0::Int) (fromLevelChars css i) initGameField
  where
  fromLevelChars css' i' = snd $ foldl (\(ai,alc) cs->(if cs=="" then ai+1 else ai,if ai==i' then alc++[cs] else alc)) (0,[]) css'
  fromRows _ [] gf = gf
  fromRows y (r:rs) gf = fromRows (y+1) rs $ fromCols 0 y r gf
    where
    fromCols _ _ [] gf' = gf'
    fromCols x' y' (c:cs) gf' = fromCols (x'+1) y' cs $ fromCell x' y' c gf'
      where
      fromCell x'' y'' '#' gf'' = gf'' & walls   .~ ((x'',y''):(gf''^.walls))
      fromCell x'' y'' '.' gf'' = gf'' & storage .~ (x'',y''):(gf''^.storage)
      fromCell _   _   ' ' gf'' = gf'' -- Floor
      fromCell x'' y'' 'o' gf'' = gf'' & crates  .~ (x'',y''):gf''^.crates
      fromCell x'' y'' '*' gf'' = gf'' & storage .~ (x'',y''):gf''^.storage
                                       & crates  .~ (x'',y''):gf''^.crates
      fromCell x'' y'' '@' gf'' = gf'' & sokoban ?~ (x'',y'') -- sokoban .~ Just (x'',y'')
      fromCell x'' y'' '+' gf'' = gf'' & storage .~ (x'',y''):gf''^.storage
                                       & sokoban ?~ (x'',y'')
      fromCell _ _ _  _         = undefined


--fileLevelReader_old ::  Int -> IO (Maybe GameField)
--fileLevelReader_old n = fmap (gameFieldFromChars 0) <$> fileLevelReaderChars' n
--  where
--      fileLevelReaderChars'::Int->IO (Maybe (Field2d Char))
--      fileLevelReaderChars' n' =
--            getDataFileName "level.txt"
--              >>= readFile
--              >>= (return . splitOnEmptyString . lines)
--              >>= (\xs->return ((V.fromList xs) V.!? n'))

fileLevelReader ::  Int -> IO (Maybe GameField)
fileLevelReader n = do
  allLevels <- allLevelsFileLevelReader
  return $ allLevels V.!? n

allLevelsFileLevelReader::IO (V.Vector GameField)
allLevelsFileLevelReader =
            getDataFileName "level.txt"
              >>= readFile
              >>= (return . splitOnEmptyString . lines)
              >>= return . V.fromList
              >>= return . fmap (gameFieldFromChars 0)

printGameField :: GameField  -> IO ()
printGameField = print' . charsFromGameField  where
  print' (c:cs) = do putStrLn ("|"++c++"|")
                     print' cs
  print' []     = return ()
