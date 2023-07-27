{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Servant
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson 
import Data.UUID
import Data.UUID.V4
import Query
import DB
import Control.Monad.IO.Class

type UserAPI1 = "health" :> Get '[JSON] Text 
    :<|> "joe" :> QueryParam "name" Text :> Get '[JSON] Text
    :<|> "foe" :> Capture "name" Text :> Get '[JSON] Text
    :<|> "postjoe" :> ReqBody '[JSON] JoeRequestBody :> Post '[JSON] Text
    :<|> "respjoe" :> ReqBody '[JSON] JoeRequestBody :> Post '[JSON] JoeRespBody
    :<|> "errjoe" :> ReqBody '[JSON] JoeRequestBody :> Post '[JSON] JoeRespBody
    :<|> "postdata" :> ReqBody '[JSON] DataRequestBody :> Post '[JSON] DataRespBody

data DataRequestBody = 
  DataRequestBody
    {
        name :: Text
      , lastName ::Text
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DataRespBody = 
  DataRespBody
    {   id :: Text
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
data JoeRequestBody = 
  JoeRequestBody 
    { name :: Text
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data JoeRespBody = 
  JoeRespBody 
    { resp_name :: Text
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


server1 :: Server UserAPI1
server1 = return "UP" 
    :<|> joeHandler
    :<|> foeHandler
    :<|> postjoeHandler
    :<|> respjoeHandler
    :<|> errjoeHandler
    :<|> postdataHandler
    -- :<|> getdataHandler
    
postdataHandler :: DataRequestBody -> Handler DataRespBody
postdataHandler DataRequestBody {..} = do 
  x <- liftIO $ toText <$> nextRandom
  let user = User x name lastName
  liftIO $ createUser user
  return $ DataRespBody x


joeHandler :: Maybe Text -> Handler Text
joeHandler j = do
    if j == Just "jyoti"
        then return "pathak"
        else return "Wrong Text joe!!"
    
foeHandler :: Text -> Handler Text  
foeHandler j = do
     if j == "jyoti"
        then return "pathak"
        else return "Wrong Text  foe !!"

postjoeHandler :: JoeRequestBody -> Handler Text
postjoeHandler JoeRequestBody {..} = do
    if name == "jyoti"
        then return "pathak"
        else return "Wrong Text  postjoe !!"

respjoeHandler :: JoeRequestBody -> Handler JoeRespBody
respjoeHandler JoeRequestBody {..} = if name == "jyoti"
                       then return $ JoeRespBody {resp_name  = "pathak"}
                       else return $ JoeRespBody {resp_name  = "wrong name"}

errjoeHandler :: JoeRequestBody -> Handler JoeRespBody
errjoeHandler JoeRequestBody {..} = if name == "jyoti"
                       then return $ JoeRespBody {resp_name  = "pathak"}
                       else throwError $ err500 { errBody = "Your request makes no sense to me." }
 

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1


