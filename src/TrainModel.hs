module TrainModel
  (
    ModelConfig(..)
  , Model(..)
  , initialize
  ) where

import Data.Array.IO (IOArray)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)


data ModelConfig = ModelConfig
  {
    numColours :: Int
  , maxTokens :: Int
  , tokenizeConfig :: TokenizeConfig
  , directory :: FilePath
  , trainData :: FilePath
  }

data Model = Model
  {
    lambda :: IOArray (Int, Int, Int) Int
  , config :: ModelConfig
  , token2id :: H.HashMap Text Int
  }

initialize :: ModelConfig -> IO Model
initialize conf = do
  let dir = saveDirectory (params config)
  catchIOError (createDirectory dir) $ \err ->
    if isAlreadyExistsError err
    then error ("The directory `" ++ dir ++ "` already exists!")
    else error (show err)

  runSqlite (T.pack $ dir ++ "/data.db") $ do
    runMigration migrateAll
    dat <- liftIO $ LB.readFile (trainData config)
    foldM addInput H.empty (decode HasHeader dat)

  where addInput ref (string :: T.Text, label :: Int, isDev :: Int) = do
          case tokenize (tokenizeConfig conf) string of
            Nothing ->
              liftIO $ putStrLn ("ERROR: Could not tokenize `" + show string + "`!")
            Just ts -> do
              liftIO $ modifyIORef' (\set -> foldl' (flip S.insert) set ts) ref
              liftIO $ putStrLn (show string ++ "(" ++ show label ++ ")" ++ ", " ++ show isDev)
        encode is x = toStrict . runPut $ do
          mapM_ put is
          mapM_ put x
