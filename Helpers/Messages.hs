module Helpers.Messages where

import Import
import Text.Blaze.Internal
import qualified Data.Text as T

data MessageType = Success
                 | Alert
                 | Error
                 | Information

successMessage :: Text -> Html
successMessage message = preEscapedText $ T.pack ("<div class=\"alert alert-success\"><a class=\"close\" data-dismiss=\"alert\"><i class=\"icon-remove\"></i></a><p><strong>Success</strong></p><p>" ++ (T.unpack message) ++ "</p></div>")

setLabel x = FieldSettings { fsLabel = SomeMessage x
                           , fsTooltip = Nothing
                           , fsId = Nothing
                           , fsName = Nothing
                           , fsAttrs = []
                           }
