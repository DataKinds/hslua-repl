module Main where

import Foreign.Lua
import System.Console.Readline
import System.IO
import qualified Data.ByteString.Char8 as B

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

handleCommands :: String    -- the input string
                  -> Lua a  -- the replLoop function
                  -> Lua ()
handleCommands luaString replLoop | luaString == "" = replLoop >> (return ())
                                  -- this is currently the only command
                                  | luaString == ":quit" = (return ())
                                  -- guaranteed not to be empty because of the first guard
                                  | (head luaString) == '=' = handleCommands ("return (" ++ (tail luaString) ++ ")") replLoop
                                  | otherwise = do
                                      eplLoop luaString
                                      replLoop
                                      return ()

main :: IO ()
main = runLua $ do
            openbase
            let replLoop = do maybeLuaString <- liftIO $ readline "Lua$ "
                              case maybeLuaString of
                                  Nothing -> replLoop >> (return ())
                                  Just str -> (liftIO $ addHistory str) >> (handleCommands str replLoop)
            replLoop
