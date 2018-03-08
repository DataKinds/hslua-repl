{-# LANGUAGE QuasiQuotes #-}

module LuaPrelude where

import NeatInterpolation
import Data.Text (unpack, Text)

luaPrelude :: String
luaPrelude = unpack [text|
function __replShow(object)
    out = ""
    if (object ~= nil) then
        if (type(object) == "string") then
            out = "\"" .. object .. "\""
        elseif (type(object) == "number") then
            out = tostring(object)
        elseif (type(object) == "function") then
            out = "function"
        elseif (type(object) == "userdata") then
            out = "userdata"
        elseif (type(object) == "table") then
            out = out .. "{\n"
            for k,v in pairs(object) do
                out = out .. "  " .. __replShow(k) .. " = " ..  __replShow(v) .. "\n"
            end
            out = out .. "}"
        end 
    end
    return out
end
function __replPrint(object)
    print("=> " .. __replShow(object))
end
|]
