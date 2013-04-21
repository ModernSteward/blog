{-# LANGUAGE RankNTypes #-}
module Helpers.Plugin where

import qualified Data.Digest.Pure.SHA as C
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Int (Int64)
import Import
import Yesod.Request ()

saveFile :: FileInfo -> IO String
saveFile file = do
    time <- getCurrentTime
    let sha = C.sha1 $ L.fromChunks [C8.pack $ T.unpack $ (fileName file) `T.append` (T.pack $ show time)]
        name = "/home/ico/upload/" ++ (show sha) ++ ".zip" 
    fileMove file name
    return name

-- pluginToJson :: forall (backend :: (* -> *) -> * -> *).
--                 Entity (PluginGeneric backend) -> Value
pluginToJson (Entity pid plugin) =
    let id' = case (fromPersistValue $ unKey pid) :: Either Text Int64 of
                   Left a -> error $ show a
                   Right a -> a
    in object [ ("title", pluginTitle plugin)
              , ("description", unTextarea $ pluginDescription plugin)
              , ("filepath", pluginFilePath plugin)
              , ("id", T.pack $ show id')
              ]
