module Main where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.State

import LuaRepl
import LuaRun
import ReplState
import Autocomplete
import LuaPrelude
import LuaRocks

main :: IO ()
main = runLua $ do
            openlibs
            opendebug
            openmath
            liftIO getLuaRocksSandbox
            replStartInfo
            runBlock luaPrelude
            setAutocomplete
            runStateT replLoop defaultReplState
            return ()
