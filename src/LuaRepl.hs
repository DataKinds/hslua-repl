module LuaRepl where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.State

import LuaRun
import ReplState
import Autocomplete
import LuaPCall
import LuaRocks

luaVersion :: Lua String
luaVersion = do
    getglobal "_VERSION"
    version <- tostring stackTop
    return $ B.unpack version

replStartInfo :: Lua ()
replStartInfo = do
    version <- luaVersion
    lR <- liftIO canAccessLuaRocks
    liftIO $ putStrLn ("hslua-repl v. 1.0.0\
    \\nCopyright 2018 Aearnus\
    \\nUses the `hslua` Haskell library to interact with " ++ version ++ ".\
    \\nType `:help` to get help.\
    \\nIs LuaRocks loaded? " ++ (show lR) ++ ".\n")

printHelp :: Lua ()
printHelp = do
    replStartInfo
    liftIO $ putStrLn ("Available commands:\n" ++ commands)
    where
        cs =
            [("quit", "Exits the interpreter."),
             ("prompt <string>", "Sets the interpreter prompt."),
             ("load <file path>", "Load a Lua file from the current directory."),
             ("reload", "Reloads the currently loaded Lua files."),
             ("globals", "Prints a list of the currently loaded globals (from _G)."),
             ("lr <command>", "Interact with LuaRocks if it is loaded. Type `:lr help` for more."),
             ("help", "Prints this text.")]
        tupleToString (cmd, desc) = ':':cmd ++ (replicate (20 - length cmd) ' ') ++ " --   " ++ desc
        commands = intercalate "\n" $ map ((++) "  ") $ map tupleToString cs

printLuaRocksHelp :: IO ()
printLuaRocksHelp = do
    lR <- canAccessLuaRocks
    putStrLn ("Is LuaRocks loaded? " ++ (show lR) ++ ".\nAvailable LuaRocks commands:\n" ++ commands)
    where
        cs =
            [("help", "Print this text."),
             ("install <rock name>", "Install a rock from LuaRocks."),
             ("search <rock name>", "Search for a rock on the LuaRocks servers.")]
        tupleToString (cmd, desc) = ":lr " ++ cmd ++ (replicate (20 - length cmd) ' ') ++ " --   " ++ desc
        commands = intercalate "\n" $ map ((++) "  ") $ map tupleToString cs


--Handles the input and output and passes control back to replLoop
handleCommands :: String                      -- the input string
                  -> StateT ReplState Lua ()
handleCommands luaString = case luaString of
    "" -> replLoop
    '=':expr -> handleCommands ("return (" ++ expr ++ ")")
    ':':cmd -> case cmd of
        "quit" -> return ()
        "help" -> do
            lift $ printHelp
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
            --handle prompt edge cases
            "prompt":_ -> do
                liftIO $ putStrLn "usage: :prompt <string>"
                replLoop
            ["load", f] -> do
                runFile f
                replLoop
            --and load edge cases
            "load":_ -> do
                liftIO $ putStrLn "usage: :load <file path>"
                replLoop
            ["lr", "help"] -> do
                liftIO printLuaRocksHelp
                replLoop
            ["lr", "install", package] -> do
                liftIO $ luaRocksInstall package
                replLoop
            ["lr", "search", package] -> do
                liftIO $ luaRocksSearch package
                replLoop
            --interpret an unrecognized lr command as a cry for help
            "lr":_ -> do
                liftIO printLuaRocksHelp
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
