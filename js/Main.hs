import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import qualified Data.JSString as S
import qualified JavaScript.Object as O
import GHCJS.Types
import Data.Maybe

import JavaScript.Extras.Cast

foreign import javascript unsafe "somethingUseful_ = $1"
    js_set_somethingUseful :: Callback a -> IO ()

somethingUseful :: JSVal -> IO JSVal
somethingUseful v = do 
    mstr <- fromJSVal v
    let str = fromMaybe (S.pack "") mstr
    (toJSVal . S.pack . reverse . S.unpack) str

returnViaArgument :: (JSVal -> IO (JSVal)) -> JSVal -> JSVal -> IO ()
returnViaArgument f arg retObj = do
    r <- f arg
    maybe (return ()) (O.setProp (S.pack "ret") r) $ fromJS retObj

main = do
    callback <- syncCallback2 ContinueAsync (returnViaArgument somethingUseful)
    js_set_somethingUseful callback
