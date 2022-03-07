import Data.Maybe
import Data.List

main :: IO ()
main = do
  putIntro
  loop [
    User "Kenobi" ["Hello there!", "If you strike me down, I shall become more powerful than you can possibly imagine."] [],
    User "Nick" ["Hi, everybody!"] []
    ]

loop :: [User] -> IO ()
loop appState = do
  command <- getLine
  if command == "quit"
    then putStrLn "Exiting. Good bye!"
    else do
    let
      mCommandFound = fmap (apply appState) (parse command)
      commandNotFound = do
        putStrLn "Command not found."
        return appState
    newState <- fromMaybe commandNotFound mCommandFound
    putStrLn $ show newState
    loop newState


apply :: [User] -> Command -> IO [User]
apply appState (Read username) = do
  let
    mUser = findUser username appState
    mOutput = fmap (putLines . messages) mUser
  fromMaybe (putStrLn "User not found.") mOutput
  return appState
  
apply appState (Post username message) = do
  let
    newUser = User username [] []
    user = fromMaybe newUser $ findUser username appState
  setUser appState $ User username (message : messages user) (followers user)

apply appState (Follow followerName followeeName) = do
  let
    mNewFollowee = do
      follower <- findUser followerName appState
      followee <- findUser followeeName appState
      return $ User (name followee) (messages followee) (followerName : followers followee)
    usersNotFound = do
      putStrLn "One or more of the specified users were not found."
      return appState 
    mNewAppState = fmap (setUser appState) mNewFollowee
  fromMaybe usersNotFound mNewAppState

apply appState (Wall userName) = do
  let
    postsOf uname = fromMaybe [] $ fmap messages $ findUser uname appState
    wallOf user = concat (messages user : map postsOf (followers user))
    mOutput = fmap (putLines . wallOf) $ findUser userName appState
  fromMaybe (putStrLn "User not found.") mOutput
  return appState


setUser :: [User] -> User -> IO [User]
setUser appState user = return $
  user : filter (\u -> name u /= name user) appState

findUser :: String -> [User] -> Maybe User
findUser username = find (\user -> name user == username)

parse :: String -> Maybe Command
parse = process . words
  where
    process [username] = Just $ Read username
    process (username : "->" : message) = Just $ Post username $ unwords message
    process [follower, "follows", followee] = Just $ Follow follower follower
    process [username, "wall"] = Just $ Wall username
    process _ = Nothing


putIntro :: IO ()
putIntro = putLines [
  "-----",
  "Hello! Welcome to our CLI. Commands are as follows:",
  "Post: <user> -> <message>",
  "Read: <user>",
  "Follow: <user> follows <other user>",
  "Wall: <user> wall",
  "Quit: quit",
  "-----"
  ]

putLines :: [String] -> IO ()
putLines lines = do
  sequence $ map putStrLn lines
  return ()


data User = User { name :: String, messages :: [String], followers :: [String] }
  deriving Show

data Command =
  Post String String |
  Read String |
  Follow String String |
  Wall String
