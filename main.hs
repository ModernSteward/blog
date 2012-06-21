import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
{-
main = do
    conf <- fromArgs parseExtra
    logger <- defaultDevelopmentLogger
    app <- makeApplication conf logger
    run "test app" app
-}
