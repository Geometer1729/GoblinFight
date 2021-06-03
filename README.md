# Goblin Fight

# Running Goblin Fight

Goblin Fight can be run with
```
stack run AI AI WORLD
```
where AI AI and WORLD are the names of the 2 AIs to use and world to load them into.

currently available worlds are:

* 2Gob
* big


currently available AIs are:

* simple

    a fairly simple native AI which attacks anyone it can, and if it's unable to attack moves toward the
closest enemy.

* cli

    not really an AI, but when cli is used that AI's team can be controlled by the command line interface.
* test.py

    an ai written in python which just stands there and doesn't take reactions. It was written to verify that the system for writing ais in other languages works.
* gloss

    Not  yet implemented but intended to behave like cli but via a gui.

# Adding new AIS

## Native AIS

To add a new native AI create a haskell file in `./ais/AIS/Natives/`
The name of the haskell module will be the name of the ai.
The haskell module must provide two functions, the first is of type `World -> Action` and should share the name of your module.
This function decides what action to take on your AI's turn.


The second is of type `CUID -> ReactionTrigger -> World -> Maybe Action` its name should be the same as the first followed by React ie. `simpleReact` this function will determine how and if to use your reactions. The CUID provided will be the id of one of your creatures, and the reaction trigger will be a reaction trigger that creature has at least one reaction for.

## Foreign AIS

Foreign AIs should be executables stored in the, `./ais/AIS/Executables` folder.
They take input from stdin which will either be a world in which case they should respond with
an action. Or they will receive "Reaction:" followed by the cuid of the creature which may use a reaction, then the reaction trigger, and finally the state of the world. In this case they should respond with a Maybe Action, meaning either "Nothing" or "Just " followed by an action. Indicating if and how they will use their reaction.

