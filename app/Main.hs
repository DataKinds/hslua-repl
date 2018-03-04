module Main where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.State.Lazy

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
        commands = intercalate "\n" $ map (\tuple -> (++) "    " $ (fst tuple) ++ "   --   " ++ (snd tuple)) cs

prompt :: Bool -> State String String
prompt willUpdate = do
    p <- get
    if (willUpdate) then
        do
            put p
            return p
    else
        return p
setPrompt :: String -> String
setPrompt p = execState (prompt True) p
getPrompt :: String
getPrompt = execState (prompt False) ""

handleCommands :: String    -- the input string
                  -> Lua a  -- the replLoop function
                  -> Lua ()
handleCommands luaString replLoop | luaString == "" = replLoop >> return ()
                                  -- this is currently the only command
                                  | luaString == ":quit" = return ()
                                  | luaString == ":help" = (luaVersion >>= printHelp) >> replLoop >> return ()
                                  | take 7 luaString == ":prompt" = do
                                      let _ = setPrompt (drop 8 luaString)
                                      replLoop
                                      return ()
                                  -- guaranteed not to be empty because of the first guard
                                  | (head luaString) == '=' = handleCommands ("return (" ++ (tail luaString) ++ ")") replLoop
                                  | otherwise = do
                                      eplLoop luaString
                                      replLoop
                                      return ()

main :: IO ()
main = runLua $ do
            openbase
            let _ = setPrompt "Lua$ "
            let replLoop = do maybeLuaString <- liftIO $ readline getPrompt
                              case maybeLuaString of
                                  Nothing -> replLoop >> (return ())
                                  Just str -> (liftIO $ addHistory str) >> (handleCommands str replLoop)
            replLoop
