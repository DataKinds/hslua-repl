module Autocomplete where

import Foreign.Lua
import qualified Data.ByteString.Char8 as B

getNames :: Lua [String]
getNames = do
    getglobal "__replGlobalNames()"
    call 0 1
    -- now, the stack is populated with a name table and nothing else important
    globalNameLen <- rawlen stackTop
    sequence $ map (getNthString) [1..(fromIntegral globalNameLen)]
    where
        getNthString n = do
            pushinteger n
            gettable (nthFromTop 2)
            (tostring stackTop) >>= (return . B.unpack)

autocomplete :: Maybe (String -> IO [String])
autocomplete = Just (\_ -> return ["XD"])
