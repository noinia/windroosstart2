import Prelude              (IO, (>>=))
import Yesod.Default.Config (fromArgs)
import Settings             (parseExtra)
import Application          (createUserApp)

main :: IO ()
main = fromArgs parseExtra >>= createUserApp
