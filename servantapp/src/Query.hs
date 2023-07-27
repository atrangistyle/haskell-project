module Query where

import Database.Beam.Postgres
import DB
import System.Environment
import Data.Maybe
import Database.Beam.Query
import Text.Read
import Data.Text



-- listofUsers <-
--   runSelectReturningList $
--   select $ filter_ (\p -> _user_id p ==. 10 &&. (_user_name p == "Smith" ||. _last_name p == "Will")) $
--   all_ (_usersTable currentDb)

-- putStrLn "Inserted playlists:"
-- forM_ insertedPlaylists $ \p ->
--   putStrLn (show p)

createUser ::  User -> IO Bool
createUser user = do
    configs <- postgresConfig
    conn <- connect configs
    runBeamPostgres conn $ do
        runInsert $ 
            insert
            (_usersTable currentDb)
            (userinsertExpressions [user])
    return True
    
getUser :: Text -> IO (Maybe User)
getUser user_id = do
    configs <- postgresConfig
    conn <- connect configs
    runBeamPostgres conn $ do
        runSelectReturningOne $
           select $ filter_ (\x -> _user_id x ==. val_ user_id ) $ (all_ (_usersTable currentDb)) -- val_ use to convert any type to Columnar(B.C)
     -- no need to return anything , runBeamPostgres will return automativally


postgresConfig ::  IO ConnectInfo
postgresConfig = do
    host <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
    port <- fromMaybe 5432 . (>>= readMaybe) <$> lookupEnv "PORT"
    user <- fromMaybe "jyotipathak" <$> lookupEnv "USER"
    password <- fromMaybe "" <$> lookupEnv "PASSWORD"
    db <- fromMaybe "mydb" <$> lookupEnv "DB_NAME"
    return $
        ConnectInfo
            { connectHost = host
            , connectPort = port
            , connectUser = user
            , connectPassword = password
            , connectDatabase = db
            }
