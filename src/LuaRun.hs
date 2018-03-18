module LuaRun where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import System.Directory
import Control.Monad.State
import Control.Monad
import Debug.Trace

import ReplState
import LuaPCall

runLine :: String -> Lua ()
runLine input = do
    getglobal "__replPrint"
    status <- loadstring input
    case status of
        OK -> do
            runtimeStatus <- handlePCall 0 1
            case runtimeStatus of -- call the loaded function
                OK -> call 1 0 -- print the result
                _ -> return ()
        Yield -> do
            liftIO $ putStrLn "runLine yield"
            return ()
        ErrSyntax -> printError "SYNTAX"
        ErrMem -> printError "OUT OF MEMORY"
        ErrGcmm -> printError "GARBAGE COLLECTOR"
        ErrFile -> printError "FILE"
        ErrRun -> printError "RUNTIME"
        ErrErr -> printError "ERROR"
    where
        printError errorType = do
            luaError <- tostring stackTop
            liftIO $ putStrLn (errorType ++ " ERROR\n    Lua:" ++ B.unpack luaError)


runBlock :: String -> Lua Status
runBlock input = do
    status <- loadstring input
    case status of
        OK -> do
            void $ handlePCall 0 1 -- call the loaded function
        Yield -> do
            void $ handlePCall 0 1
        ErrSyntax -> printError "SYNTAX"
        ErrMem -> printError "OUT OF MEMORY"
        ErrGcmm -> printError "GARBAGE COLLECTOR"
        ErrFile -> printError "FILE"
        ErrRun -> printError "RUNTIME"
        ErrErr -> printError "ERROR"
    return status
    where
        printError errorType = do
            luaError <- tostring stackTop
            liftIO $ putStrLn (errorType ++ " ERROR IN LUA BLOCK\n    Lua:" ++ B.unpack luaError)

runFile :: FilePath -> StateT ReplState Lua ()
runFile fileName = do
    fileBool <- liftIO $ doesFileExist fileName
    if fileBool then do
        --if the file exists
        luaFile <- liftIO $ readFile fileName
        liftIO $ putStrLn ("Loaded Lua file " ++ fileName)
        luaStatus <- lift $ runBlock luaFile
        case luaStatus of
            OK -> do
                liftIO $ putStrLn "Successfully loaded."
                modify (updateLoadedFiles fileName)
                rS <- get
                let filesToPrompt = intercalate " " $ loadedFiles rS
                modify (updateReplPrompt ("Lua " ++ filesToPrompt ++ "$ "))
            Yield -> do
                liftIO $ putStrLn "Successfully loaded, yielded into coroutine."
            _ -> do
                liftIO $ putStrLn "Failed to load Lua file."
    else
        --if the file doesn't exist
        liftIO $ putStrLn ("Lua file " ++ fileName ++ " not found.")
