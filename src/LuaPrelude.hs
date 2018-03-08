{-# LANGUAGE QuasiQuotes #-}

module LuaPrelude where

import NeatInterpolation
import Data.Text (unpack, Text)

luaPrelude :: String
luaPrelude = unpack [text|
function __replPrint(object)
    if (object ~= nil) then
        print("=> " .. object)
    end
end
|]
