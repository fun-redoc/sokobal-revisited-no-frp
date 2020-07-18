# Sokoban revisited

partly rewritten using pure gloss insted of frp as in the [2014 Version](
https://github.com/fun-redoc/sokoban)

Implemented Ascii and Gloss version.

**Start Ascii:** stack run sokoban-ascii-exe  
**Start Gloss:** stack run sokoban-gloss-exe

## Levels

Orignal levels probably copyrighted. You will have to create your own levels or
search for open sourced community levels.  
This Version reads the standard sokoban level format as described in wikipedia.  
Level definitions has to be put in `data/level.txt` file.  
`data/level.txt` is not provided in this version.


## Keys
* `wasd` or `hjkl`: for movement
* `u`: undo move
* `r`: restart level
* `q`: quit (last level ist saved in `~/.sokobanLastLevel.txt`)

**Hint**:  
Restarting from Level 0, delete `~/.sokobanLastLevel.txt`

## TODO:
* interpret command line params (e.g. for ascii or ui mode)
* make man movement smooth
* animate man
* score count
* high score
* reset to initial Level
* add surely open sourced levels
* *very hard*: procedural level genenrator
