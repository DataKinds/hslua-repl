module Main where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.State.Lazy

newtype ReplState = ReplState {
    replPrompt :: String
}
defaultReplState :: ReplState
defaultReplState = ReplState { replPrompt = "Lua$ " }
updateReplPrompt :: String -> ReplState -> ReplState
updateReplPrompt p rS = rS { replPrompt = p }


eplLoop :: String -> Lua ()
eplLoop input = do
    getglobal "print"
    status <- loadstring input
    case status of
        OK -> do
            call 0 1 -- call the loaded function
            call 1 0 -- print the result
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
            [(":quit", "Exits the interpreter."),
             (":prompt", "Sets the interpreter prompt."),
             (":help", "Prints this text.")]
        tupleToString (cmd, desc) = cmd ++ (replicate (12 - length cmd) ' ') ++ " --   " ++ desc
        commands = intercalate "\n" $ map ((++) "    ") $ map tupleToString cs

--Handles the input and output and passes control back to replLoop
handleCommands :: String                      -- the input string
                  -> StateT ReplState Lua ()  -- the replLoop function
                  -> StateT ReplState Lua ()
handleCommands luaString replLoop | luaString == "" = runReplLoop >> return ()
                                  | luaString == ":quit" = return ()
                                  | luaString == ":help" = do
                                      let v = luaVersion
                                      let _ = v >>= printHelp
                                      runReplLoop
                                      return ()
                                  | take 7 luaString == ":prompt" = do
                                      modify (updateReplPrompt (drop 8 luaString))
                                      runReplLoop
                                      return ()
                                  -- guaranteed not to be empty because of the first guard
                                  | (head luaString) == '=' = handleCommands ("return (" ++ (tail luaString) ++ ")") replLoop
                                  | otherwise = do
                                      let _ = eplLoop luaString
                                      runReplLoop
                                      return ()
                                  where
                                      runReplLoop = do
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
            handleCommands str replLoop

main :: IO ()
main = runLua $ do
            openbase
            runStateT replLoop defaultReplState
            return ()
