module Autocomplete where

import Foreign.Lua
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.Reader
import System.Console.Readline (setCompletionEntryFunction, filenameCompletionFunction)

dumpStack :: Lua ()
dumpStack = do
    n <- gettop
    ss <- sequence $ map (stackString . nthFromTop) [1..(fromStackIndex n)]
    liftIO $ putStrLn "Stack:"
    liftIO $ mapM_ (putStrLn) ss
    where
        stackString si = do
            t <- ltype si
            ts <- typename t
            s <- tostring si
            return ("type: " ++ ts ++ "    value: " ++ (B.unpack s))

getNames :: LuaState -> IO [String]
getNames = runReaderT (unLua $ do
    getglobal "__replGlobalNames"
    call 0 1
    -- now, the stack is populated with a name table and nothing else important
    globalNameLen <- rawlen stackTop
    names <- sequence $ map getNthString [1..(fromIntegral globalNameLen)]
    -- before finishing this function, clean the stack
    -- pop the table we pushed at the start
    pop 1
    return names)
    where
        getNthString n = do
            -- assume table is on top of stack
            pushinteger n
            gettable (nthFromTop 2)
            s <- tostring stackTop
            pop 1
            (return . B.unpack) s

autocomplete :: [String] -> String -> [String]
autocomplete names s = filter (isPrefixOf s) names

autocompleteWrapper :: LuaState -> String -> IO [String]
autocompleteWrapper ls s = do
    names <- getNames ls
    filenames <- filenameCompletionFunction s
    return $ (autocomplete names s) ++ filenames

setAutocomplete :: Lua ()
setAutocomplete = do
    luaStatePtr <- luaState
    liftIO $ setCompletionEntryFunction $ Just (autocompleteWrapper luaStatePtr)
