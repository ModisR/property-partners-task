# property-partners-task
Coding assignment for technical interview with Property Partners

## Building & Running
1. Install the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) by running the command `sudo apt install ghc`.
2. Navigate to this project's directory and compile with `ghc Main.hs`.
3. Run the executable with `./Main`.

## Code Structure
The entry point to the program is an IO action which does 2 things, print a brief command summary and initiate the command loop with an empty list of users
### Main
```hs
main :: IO ()
main = putIntro >> loop []
```
### Command Loop
At the core of the program is this recursive function. It accepts the state of the app, a list of `User`s, and returns an input/output action:
```hs
loop :: [User] -> IO ()
loop appState = do
  line <- getLine
  if line == "quit"
    then putStrLn "Exiting. Good bye!"
    else do
    let
      execute Nothing = do
        putStrLn "Command not found."
        return appState
      execute (Just command) = apply command appState
    newAppState <- execute $ parse line
    loop newAppState
```
It reads a single line from standard input and first checks for the "quit" command, in which case it ends the program.
Otherwise, it attempts to `parse` the line into a `Command`.
If successful, that `Command` is applied to the `appState`, potentially mutating it or resulting in some terminal output.
If failed, the user is informed and the `appState` remains unchanged.
Either way, the loop is then invoked again with the resulting `newAppState`, ready to try another command.

### Applying Commands
Because applying a command involves a mixture of both modifying app state and input/output, the type of an app state change operation is `[User] -> IO [User]`.
We handle each of the 4 cases with a pattern match.

#### Read
```hs
apply :: Command -> [User] -> IO [User]
apply (Read username) appState = do
  let
    viewPostsOf Nothing = putNotFound username
    viewPostsOf (Just user) = putLines $ map show $ messages user
  viewPostsOf $ findUser username appState
  return appState
```
In the `let` block, declare `viewPostsOf`, handling 2 cases:
- no user found - then put an error message to the console.
- a user is found, in which case print all their authored messages.

Then, apply it to the outcome of a username search before returning the unmodified `appState`.

#### Post
```hs
apply (Post username message) appState = do
  time <- getCurrentTime
  let
    msg = Message message time 
    newUser = User username [] []
    user = fromMaybe newUser $ findUser username appState
  setUser (User username (msg : messages user) (followees user)) appState
```
First, extract the system time and bind it to `time`.

In the `let` block:
1. Stamp the provided `message` with the `time`.
2. Create a blank user with the provided `username` in case they don't yet exist.
3. Maybe find a user with that name and get them or else use the blank `newUser`.

Finally, add the time-stamped `Message` to the head of the `user`'s list of `messages` in the expression `(User username (msg : messages user) (followees user))`
and update the `appState`.

#### Follow
```hs
apply (Follow followerName followeeName) appState =
  addFollowing (findUser followerName appState) (findUser followeeName appState)
  where
    addFollowing Nothing Nothing = do
      putNotFound followerName
      putNotFound followeeName
      return appState
    addFollowing Nothing _ = do
      putNotFound followeeName
      return appState
    addFollowing _ Nothing = do
      putNotFound followerName
      return appState
    addFollowing (Just follower) (Just followee) = setUser newFollower appState
      where
        newFollower = User (name follower) (messages follower) (followee : followees follower)
```
We try to find each user specified in the command, the follower and follower, then handle each of the 4 possible combinations of finding them or not,
printing new error messages or updating the `appState` with the new following as appropriate.

#### Wall
```hs
apply (Wall userName) appState = do
  let
    putWallOf Nothing = putNotFound userName
    putWallOf (Just user) = putLines wall
      where
        wall = map toString sortedMessages
        toString (author, msg) = author ++ " - " ++ show msg
        sortedMessages = reverse $ sortBy compareTimes messagesOnWall
        compareTimes (_, msg1) (_, msg2) = compare (time msg1) (time msg2)
        messagesOnWall = do
          userOnWall <- usersOnWall
          let addAuthor msg = (name userOnWall, msg)
          map addAuthor $ messages userOnWall
        usersOnWall = user : catMaybes followeeMaybes
        followeeMaybes = map (`findUser` appState) (followees user)
  putWallOf $ findUser userName appState
  return appState
```
Views the aggregated feed of a user and everyone they follow. In the `let` block, we first declare an error message must be written if the user is not found.
Otherwise, we print each line in the `wall` where the `wall` is a list of messages which has been tagged with the name of their author, sorted by time,
then prettified to a string. That list is drawn from the user themselves and their list of `followees`.

  
### Parsing
Because not all strings are valid commands, this function returns a `Maybe Command`.
It starts by splitting the line into a list of `words` and matching them to one of 4 patterns corresponding to each `Command`.
```hs
parse :: String -> Maybe Command
parse = process . words
  where
    process [username] = Just $ Read username
    process (username : "->" : message) = Just $ Post username $ unwords message
    process [follower, "follows", followee] = Just $ Follow follower followee
    process [username, "wall"] = Just $ Wall username
    process _ = Nothing
```

### App State Management
```hs
setUser :: User -> [User] -> IO [User]
setUser user appState = return $
  user : filter (\u -> name u /= name user) appState

findUser :: Username -> [User] -> Maybe User
findUser username = find $ (\u -> username == name u)
```

### Data Types

```hs
data User = User { name :: Username, messages :: [Message], followees :: [User] }

data Message = Message { body :: String, time :: UTCTime }

instance Show Message where
  show msg = body msg ++ "    (" ++ show (time msg) ++ ")"

data Command =
  Post Username String |
  Read Username |
  Follow Username Username |
  Wall Username

type Username = String
```

#### User
A list of these describes the entire state of the app. They each have a name, list of `Message`s they authored, and a list of whom they follow.

#### Message
Each has a `body` and a `time` stamp for chronological sorting, along with a `Show` type class instance for pretty printing.

#### Username
A type synonym to help semantically distinguish Strings which may refer to users from ones which never do.

#### Command
This data type encodes 4 possible actions:
- posting a message as a specified user
- reading all messages of a specified user
- making one user a follower of another
- viewing the "wall" of a user, an aggregated list of all messages of theirs and their followees'.
