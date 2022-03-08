# property-partners-task
Coding assignment for technical interview with Property Partners

## Building & Running
1. Install the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) by running the command `sudo apt install ghc`.
2. Navigate to this project's directory and compile with `ghc Main.hs`.
3. Run the executable with `./Main`.

## Code Structure
### Command Loop
At the core of the program is this recursive function. It accepts the state of the app, a list of `User`s, and returns an input/output action:
```hs
loop :: [User] -> IO ()
loop appState = do
  command <- getLine
  if command == "quit"
    putStrLn "Exiting. Good bye!"
  else do
    let
      mCommandFound = fmap (apply appState) (parse command)
      commandNotFound = do
        putStrLn "Command not found."
        return appState
    newAppState <- fromMaybe commandNotFound mCommandFound
    loop newAppState
```
It reads a single line from standard input, binds it to the variable `command` and checks whether it received the word "quit".
If yes, the app terminates. Otherwise, it attempts to `parse` the input into a `Command`.
If successful, that `Command` is applied to the `appState`, potentially mutating it or resulting in some terminal output.
If failed, the user is informed and the appState remains unchanged.
Either way, the loop is then invoked again with the resulting `appState`, ready to try a new command.

### The Command Type
This data type encodes 4 possible actions:
- posting a message as a specified user
- reading all messages of a specified user
- making one user a follower of another
- viewing the "wall" of a user, an aggregated list of all messages of theirs and their followees'.
```hs
data Command =
  Post Username Message |
  Read Username |
  Follow Username Username |
  Wall Username
```
