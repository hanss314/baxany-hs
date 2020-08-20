import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import qualified Data.JSString as S
import qualified JavaScript.Object as O
import GHCJS.Types
import Data.Maybe
import JavaScript.Extras.Cast

import Text.JSON
import Json
import Board
import Move


foreign import javascript unsafe "movesAt_ = $1"
    js_set_movesAt :: Callback a -> IO ()

--movesAt :: Board -> Pos -> [Move]

valToObj :: (JSON a) => JSVal -> IO a
valToObj v = do
    mstr <- fromJSVal v >>= (return . maybe "" S.unpack)
    let parsed = decode mstr
    case parsed of
        Ok x -> return x
        Error e -> fail e

objToVal :: (JSON a) => a -> IO JSVal
objToVal o = toJSVal $ S.pack $ encode o


jsMovesAt :: JSVal -> JSVal -> IO JSVal
jsMovesAt b p = do
    board <- valToObj b
    pos <- valToObj p
    objToVal $ movesAt board pos

returnViaArgument :: (JSVal -> IO JSVal) -> JSVal -> JSVal -> IO ()
returnViaArgument f arg retObj = do
    r <- f arg
    maybe (return ()) (O.setProp (S.pack "ret") r) $ fromJS retObj

returnViaArgument2 :: (JSVal -> JSVal -> IO JSVal) -> JSVal -> JSVal -> JSVal -> IO ()
returnViaArgument2 f a1 a2 retObj = do
    r <- f a1 a2
    maybe (return ()) (O.setProp (S.pack "ret") r) $ fromJS retObj

main = do
    movesAtCb <- syncCallback3 ContinueAsync (returnViaArgument2 jsMovesAt)
    js_set_movesAt movesAtCb 
