module LuaPCall where

import Foreign.Lua
import qualified Data.ByteString.Char8 as B

handlePCall :: NumArgs -> NumResults -> Lua Status
handlePCall nargs nresults = do
    status <- pcall nargs nresults Nothing
    case status of
        OK -> return status
        _ -> do
            (tostring stackTop) >>= (liftIO . B.putStrLn)
            pop 1
            return status
