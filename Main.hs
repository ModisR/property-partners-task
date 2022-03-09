import Data.Maybe
import Data.List
import Data.Time.Clock

main :: IO ()
main = putIntro >> loop []

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


apply :: Command -> [User] -> IO [User]
apply (Read username) appState = do
  let
    viewPostsOf Nothing = putNotFound username
    viewPostsOf (Just user) = putLines $ map show $ messages user
  viewPostsOf $ findUser username appState
  return appState
  
apply (Post username message) appState = do
  time <- getCurrentTime
  let
    msg = Message message time 
    newUser = User username [] []
    user = fromMaybe newUser $ findUser username appState
  setUser (User username (msg : messages user) (followees user)) appState

apply (Follow followerName followeeName) appState =
  addFollowing (findUser followerName appState) (findUser followeeName appState)
  where
    addFollowing Nothing Nothing = do
      putNotFound followerName
      putNotFound followeeName
      return appState
    addFollowing Nothing _ = do
      putNotFound followerName
      return appState
    addFollowing _ Nothing = do
      putNotFound followeeName
      return appState
    addFollowing (Just follower) (Just followee) = setUser newFollower appState
      where
        newFollower = User (name follower) (messages follower) (followeeName : followees follower)

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


setUser :: User -> [User] -> IO [User]
setUser user appState = return $
  user : filter (\u -> name u /= name user) appState

findUser :: Username -> [User] -> Maybe User
findUser username = find $ (\u -> username == name u)


parse :: String -> Maybe Command
parse = process . words
  where
    process [username] = Just $ Read username
    process (username : "->" : message) = Just $ Post username $ unwords message
    process [follower, "follows", followee] = Just $ Follow follower followee
    process [username, "wall"] = Just $ Wall username
    process _ = Nothing


putNotFound :: Username -> IO ()
putNotFound username = putStrLn $ "User " ++ username ++ " not found."

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

data User = User { name :: Username, messages :: [Message], followees :: [Username] }

data Message = Message { body :: String, time :: UTCTime }

instance Show Message where
  show (Message body time) = body ++ "    (" ++ show time ++ ")"

data Command =
  Post Username String |
  Read Username |
  Follow Username Username |
  Wall Username

type Username = String
