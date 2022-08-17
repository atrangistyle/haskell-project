{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Servant
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson 

type UserAPI1 = "health" :> Get '[JSON] Text 
    :<|> "joe" :> QueryParam "name" Text :> Get '[JSON] Text
    :<|> "foe" :> Capture "name" Text :> Get '[JSON] Text
    :<|> "postjoe" :> ReqBody '[JSON] JoeRequestBody :> Post '[JSON] Text
    :<|> "respjoe" :> ReqBody '[JSON] JoeRequestBody :> Post '[JSON] JoeRespBody

data JoeRequestBody = 
  JoeRequestBody 
    { name :: Text
    }
  deriving (Eq, Show, Generic, FromJSON)

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
postjoeHandler j = do
    if name j == "jyoti"
        then return "pathak"
        else return "Wrong Text  postjoe !!"

respjoeHandler :: JoeRequestBody -> Handler JoeRespBody
respjoeHandler req = if name req == "jyoti"
                       then return $ JoeRespBody {resp_name  = "pathak"}
                       else return $ JoeRespBody {resp_name  = "wrong name"}

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1


