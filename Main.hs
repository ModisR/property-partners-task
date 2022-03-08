import Data.Maybe
import Data.List
import Data.Time.Clock

main :: IO ()
main = do
  putIntro
  time <- getCurrentTime
  let
    nick = User "Nick" [Message "Hi, everybody!" time] []
    pika = User "Pikachu" [Message "Pika" time, Message "Pika Pika" time, Message "Pikachuuuu!" time] []
  loop [
    User "Kenobi" [Message "Hello there!" time, Message "You were my brother, Annakin!" time] [nick, pika],
    pika,
    nick]

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
    mOutput = fmap (putLines . map show . messages) mUser
  fromMaybe (putStrLn "User not found.") mOutput
  return appState
  
apply appState (Post username message) = do
  let
    newUser = User username [] []
    user = fromMaybe newUser $ findUser username appState
  message <- fmap (Message message) getCurrentTime
  setUser (User username (message : messages user) (followees user)) appState

apply appState (Follow followerName followeeName) = do
  let
    mNewFollower = do
      follower <- findUser followerName appState
      followee <- findUser followeeName appState
      return $ User (name follower) (messages follower) (followee : followees follower)
    usersNotFound = do
      putStrLn "One or more of the specified users were not found."
      return appState 
    mNewAppState = fmap (`setUser` appState) mNewFollower
  fromMaybe usersNotFound mNewAppState

apply appState (Wall userName) = do
  let
    postsOf user = map (\msg -> name user ++ " - " ++ show msg ) (messages user)
    wallOf user = concat (map show (messages user) : map postsOf (followees user))
    mOutput = fmap (putLines . wallOf) $ findUser userName appState
  fromMaybe (putStrLn "User not found.") mOutput
  return appState


setUser :: User -> [User] -> IO [User]
setUser user appState = return $
  user : filter (\u -> name u /= name user) appState

findUser :: String -> [User] -> Maybe User
findUser username = find $ (\u -> username == name u)


parse :: String -> Maybe Command
parse = process . words
  where
    process [username] = Just $ Read username
    process (username : "->" : message) = Just $ Post username $ unwords message
    process [follower, "follows", followee] = Just $ Follow follower followee
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
  "-----"]

putLines :: [String] -> IO ()
putLines = mapM_ putStrLn

data User = User { name :: Username, messages :: [Message], followees :: [User] } deriving Show

data Message = Message { body :: String, time :: UTCTime }

instance Show Message where
  show msg = body msg ++ " (" ++ show (time msg) ++ ")"

data Command =
  Post Username String |
  Read Username |
  Follow Username Username |
  Wall Username

type Username = String
