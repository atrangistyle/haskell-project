{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant
import           Servant.Client
import           Servant.Server
import           Lib


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   around withUserApp $ do
    -- create a test client function
    let (upHandler :<|> joeHandler :<|> foeHandler :<|> postjoeHandler :<|> respjoeHandler  :<|> errjoeHandler) = client (Proxy :: Proxy UserAPI1)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "My Test" $ do
        describe "GET /health" $ do
            it "expected : UP Success" $ \port -> do
                result <- runClientM upHandler (clientEnv port)
                result `shouldBe` (Right "UP")

        describe "GET /joeHandler" $ do
            it "expected : pathak as Response" $ \port -> do
                result <- runClientM (joeHandler $ Just "jyoti") (clientEnv port)
                result `shouldBe` (Right "pathak")
            it "expected : Wrong Text joe!!  as Response" $ \port -> do
                result <- runClientM (joeHandler $ Just "haskell") (clientEnv port)
                result `shouldBe` (Right "Wrong Text joe!!")

        describe "GET /foeHandler" $ do
            it "expected : pathak as Resp" $ \port -> do
                result <- runClientM (foeHandler "jyoti") (clientEnv port)
                result `shouldBe` (Right "pathak")
            it "expected : Wrong Text foe!!  as Resp" $ \port -> do
                result <- runClientM (foeHandler "haskell") (clientEnv port)
                result `shouldBe` (Right "Wrong Text  foe !!")

        describe "GET /postjoeHandler" $ do
            it "expected : pathak as Resp" $ \port -> do
                result <- runClientM (postjoeHandler  $ JoeRequestBody "jyoti" ) (clientEnv port)
                result `shouldBe` (Right "pathak")
            it "expected : Wrong Text postjoe!!  as Resp" $ \port -> do
                result <- runClientM (postjoeHandler $ JoeRequestBody "haskell") (clientEnv port)
                result `shouldBe` (Right "Wrong Text  postjoe !!")
        
        describe "GET /respjoeHandler" $ do
            it "expected : pathak as Resp" $ \port -> do
                result <- runClientM (respjoeHandler  $ JoeRequestBody "jyoti" ) (clientEnv port)
                result `shouldBe` (Right $ JoeRespBody "pathak")
            it "expected : Wrong name!!  as Resp" $ \port -> do
                result <- runClientM (respjoeHandler $ JoeRequestBody "haskell") (clientEnv port)
                result `shouldBe` (Right $ JoeRespBody "wrong name")

        describe "GET /errjoeHandler" $ do
            it "expected : pathak as Resp" $ \port -> do
                result <- runClientM (errjoeHandler  $ JoeRequestBody "jyoti" ) (clientEnv port)
                result `shouldBe` (Right $ JoeRespBody "pathak")
            it "expected : Your request makes no sense to me  as Resp" $ \port -> do
                result <- runClientM (errjoeHandler $ JoeRequestBody "haskell") (clientEnv port)
                case result of
                    Left (FailureResponse _ resp) -> Servant.Client.responseBody resp `shouldBe` "Your request makes no sense to me."
                    _ -> 1 `shouldBe` 2



withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure app1) action