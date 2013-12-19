Introduction
======

Morphy is a Haskell implementation of the paper and pencil game Morpion Solitaire. User will be able to play this game with a command line interface.

Morpion Solitaire is a game which is played on a plus-shaped grid of dots, and has been the subject of mathematical studies. Being a NP-hard problem, there are also some attempts to use computer to search for solutions.

For more information on this game, visit http://www.morpionsolitaire.com/.

Authors
=====

- Chenxi Wang <chenxiw@seas.upenn.edu>
- Siyu Song <siyusong@seas.upenn.edu>

Files
====

- Main.hs - Entry point. Currently calls main method in CLI module.
- CLI.hs - Command Line Interface Code.
- BoardPrinter.hs - Functions for pretty printing a game board. Mainly used in CLI module.
- GameLogic.hs - Core game logic Code.
- Test.hs - HUnit tests and QuickCheck properties.

Components
==========

1. Core Game Logic
  - Provides basic data structures (point, lines, and board).
  - Contains all operations in a game: make a move, undo a move, and so on
  - Contains functions for validating a move, a board, and a sequence of moves
  - Contains functions for serializing and parsing game state
  - Able to find all possible moves given any board 

2. CLI
  - Represent the board (crosses and lines) elegantly in CL 
  - Friendly Interface 
  - Users can:
    - Save/load a game
    - Make a move (draw a line)
    - Undo a move
    - Random play
    - Replay
    - Make choices when theres ambiguity (i.e. more than one move possible on the selected point)
  - Implemented using State Monad

3. Parser
  - Exports the game state to a plain text file, which could be easily understood by the user in the Morpion Solitaire community.
  - Parses a save file and restore the game state.
  - The game state is validated immediately after imported, and the user will be notified if the game cannot be replayed. 
  - QuickCheck'ed using the property that a exported game could be imported back without loss of information

Libraries
=======

- base
- containers
- mtl
- ansi-terminal
- random
- parsec
- HUnit
- QuickCheck

(see cabal file for details)