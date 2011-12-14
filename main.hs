import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withCS)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withCS