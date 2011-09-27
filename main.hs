import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withCS)

main :: IO ()
main = defaultMain fromArgs withCS