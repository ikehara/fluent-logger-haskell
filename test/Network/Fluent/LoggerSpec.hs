{-# LANGUAGE OverloadedStrings #-}
module Network.Fluent.LoggerSpec ( spec ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.ByteString ( ByteString )
import Data.MessagePack
import Test.Hspec

import Network.Fluent.Logger
import MockServer

spec :: Spec
spec = do
  describe "post" $ do
    it "posts a message" postPostsMessage
    it "keeps message order" postKeepsMessageOrder
    it "buffers message if server down" postBuffersMessageIfServerDown
    -- it "buffers message if lost connection" postBuffersMessageIfLostConnection
    -- it "losts message if buffer is over" postLostsMessageIfBufferIsOver
  describe "postWithTime" $ do
    it "posts a message with given time" postWithTimePostsMessageWithGivenTime


postSettings =
    defaultFluentSettings { fluentSettingsTag = "post"
                          , fluentSettingsHost = mockServerHost
                          , fluentSettingsPort = mockServerPort
                          }

postWithTimeSettings =
    defaultFluentSettings { fluentSettingsTag = "postWithTime"
                          , fluentSettingsHost = mockServerHost
                          , fluentSettingsPort = mockServerPort
                          }

getCurrentEpochTime :: IO Int
getCurrentEpochTime = round <$> getPOSIXTime

postPostsMessage :: IO ()
postPostsMessage =
    withFluentLogger postSettings $ \logger -> do
      let label = "PostsMessage"
      withMockServer $ \server -> do
        preTime <- getCurrentEpochTime
        post logger label (ObjectString "test")
        postTime <- getCurrentEpochTime
        ObjectArray (tag:time:content:[]) <- recvMockServer server
        tag `shouldBe` (ObjectBinary "post.PostsMessage")
        content `shouldBe` (ObjectString "test")

postKeepsMessageOrder :: IO ()
postKeepsMessageOrder =
    withFluentLogger postSettings $ \logger -> do
      let label = "KeepsMessageOrder"
      withMockServer $ \server -> do
        let xs = map ObjectInt [1..1024::Int]
        mapM_ (post logger label) xs
        forM_ xs $ \n -> do
          ObjectArray (_:_:content:[]) <- recvMockServer server
          content `shouldBe` n

postBuffersMessageIfServerDown :: IO ()
postBuffersMessageIfServerDown =
    withFluentLogger postSettings $ \logger -> do
      let label = "BuffersMessageIfServerDown"
      post logger label (ObjectInt 1)
      withMockServer $ \server -> do
        post logger label (ObjectInt 2)
        ObjectArray (_:_:content:[]) <- recvMockServer server
        content `shouldBe` (ObjectInt 1)
        ObjectArray (_:_:content:[]) <- recvMockServer server
        content `shouldBe` (ObjectInt 2)

postBuffersMessageIfLostConnection :: IO ()
postBuffersMessageIfLostConnection =
    withFluentLogger postSettings $ \logger -> do
      let label = "BuffersMessageIfLostConnection"
      withMockServer $ \server -> do
        post logger label (ObjectInt 1)
        ObjectArray (_:_:content:[]) <- recvMockServer server
        content `shouldBe` (ObjectInt 1)
      post logger label (ObjectInt 2)
      withMockServer $ \server -> do
        post logger label (ObjectInt 3)
        ObjectArray (_:_:content:[]) <- recvMockServer server
        content `shouldBe` (ObjectInt 2)
        ObjectArray (_:_:content:[]) <- recvMockServer server
        content `shouldBe` (ObjectInt 3)

postLostsMessageIfBufferIsOver :: IO ()
postLostsMessageIfBufferIsOver =
    withFluentLogger postSettings $ \logger -> do
      let label = "LostsMessageIfBufferIsOver"
      post logger label (ObjectInt 1)
      withMockServer $ \server -> do
        post logger label (ObjectInt 2)
        ObjectArray (_:_:content:[]) <- recvMockServer server
        content `shouldBe` (ObjectInt 2)

postWithTimePostsMessageWithGivenTime :: IO ()
postWithTimePostsMessageWithGivenTime =
    withFluentLogger postWithTimeSettings $ \logger -> do
      let label = "PostsMessageWithGivenTime"
      withMockServer $ \server -> do
        postWithTime logger label 123456 (ObjectString "test")
        ObjectArray (tag:time:content:[]) <- recvMockServer server
        tag `shouldBe` (ObjectBinary "postWithTime.PostsMessageWithGivenTime")
        time `shouldBe` (ObjectInt 123456)
        content `shouldBe` (ObjectString "test")
