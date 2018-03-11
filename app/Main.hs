module Main where

import Foreign.Lua
import System.Console.Readline hiding (getPrompt)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad.State

import LuaRepl
import LuaFile
import ReplState
import Autocomplete
import LuaPrelude

main :: IO ()
main = runLua $ do
            openlibs
            opendebug
            openmath
            runBlock luaPrelude
            setAutocomplete
            runStateT replLoop defaultReplState
            return ()
