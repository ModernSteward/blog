module Helpers.Plugin where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy (writeFile)
--import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA
import qualified Data.Text as T
import Data.Int (Int64)
import Import

saveFile :: FileInfo -> IO String
saveFile file = do
--    time <- getCurrentTime
--    let sha = sha1 $ (fileContent file) ++ (pack $ show time)
    let sha = sha1 $ fileContent file
    let name = "static/upload/" ++ showDigest sha ++ ".zip"
    writeFile name $ fileContent file
    return name

pluginToJson (Entity pid plugin) =
    let id' = case (fromPersistValue $ unKey pid) :: Either Text Int64 of
                   Left a -> error $ show a
                   Right a -> a
    in object [ ("title", pluginTitle plugin)
              , ("description", unTextarea $ pluginDescription plugin)
              , ("filepath", pluginFilePath plugin)
              , ("id", T.pack $ show id')
              ]
