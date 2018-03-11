module LuaRepl where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.State

import LuaFile
import ReplState
import Autocomplete
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
        Yield -> return ()
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

luaVersion :: Lua String
luaVersion = do
    getglobal "_VERSION"
    version <- tostring stackTop
    return $ B.unpack version
printHelp :: String -> Lua ()
printHelp v = liftIO $ putStrLn ("hslua-repl v. 1.0.0\nCopyright 2018 Aearnus\nUses the `hslua` Haskell library to interact with " ++ v ++ ".\nAvailable commands:\n" ++ commands)
    where
        cs =
            [("quit", "Exits the interpreter."),
             ("prompt", "Sets the interpreter prompt."),
             ("load", "Load a Lua file from the current directory."),
             ("reload", "Reloads the currently loaded Lua files."),
             ("globals", "Prints a list of the currently loaded globals (from _G)."),
             ("help", "Prints this text.")]
        tupleToString (cmd, desc) = ':':cmd ++ (replicate (12 - length cmd) ' ') ++ " --   " ++ desc
        commands = intercalate "\n" $ map ((++) "    ") $ map tupleToString cs

--Handles the input and output and passes control back to replLoop
handleCommands :: String                      -- the input string
                  -> StateT ReplState Lua ()
handleCommands luaString = case luaString of
    "" -> replLoop
    '=':expr -> handleCommands ("return (" ++ expr ++ ")")
    ':':cmd -> case cmd of
        "quit" -> return ()
        "help" -> do
            lift $ luaVersion >>= printHelp
            replLoop
        "reload" -> do
            rS <- get
            let fs = loadedFiles rS
            mapM runFile fs
            liftIO $ putStrLn $ "Reloaded " ++ (show $ length fs) ++ " files."
            replLoop
        "globals" -> do
            ls <- lift luaState
            names <- lift $ liftIO $ getNames ls
            lift $ liftIO $ mapM_ putStrLn names
            replLoop
        _ -> case (words cmd) of
            ["prompt", p] -> do
                modify (updateReplPrompt (drop 8 luaString))
                replLoop
            ["load", f] -> do
                runFile f
                replLoop
            _ -> do
                liftIO $ putStrLn "Unrecognized REPL command."
                replLoop
    str -> do
        lift $ runLine str
        replLoop


-- Handles the input and output IO actions and passes control off to `handleCommands`.
replLoop :: StateT ReplState Lua ()
replLoop = do
    replState <- get
    maybeLuaString <- liftIO $ readline (replPrompt replState)
    case maybeLuaString of
        Nothing -> replLoop >> (return ())
        Just str -> do
            liftIO $ addHistory str
            handleCommands str
