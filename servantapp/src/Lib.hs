{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Servant
import Data.Text

type UserAPI1 = "health" :> Get '[JSON] Text

server1 :: Server UserAPI1
server1 = return "UP" 

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1
