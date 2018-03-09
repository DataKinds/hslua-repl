{-# LANGUAGE QuasiQuotes #-}

module LuaPrelude where

import NeatInterpolation
import Data.Text (unpack, Text)

luaPrelude :: String
luaPrelude = unpack [text|
function __replShow(object, depth)
    maxDepth = 10
    out = ""
    if (depth < maxDepth) then
        if (object ~= nil) then
            if (type(object) == "string") then
                out = "\"" .. object .. "\""
            elseif (type(object) == "number") then
                out = tostring(object)
            elseif (type(object) == "function") then
                out = "<function>"
            elseif (type(object) == "userdata") then
                out = "<userdata>"
            elseif (type(object) == "table") then
                out = out .. "{\n"
                for k,v in pairs(object) do
                    out = out .. string.rep("  ", depth + 1) .. __replShow(k, depth + 1) .. " = " ..  __replShow(v, depth + 1) .. "\n"
                end
                out = out .. string.rep("  ", depth) .. "}"
            end
        else
            out = "nil"
        end
    else
        out = "... <max print depth> ..."
    end
    return out
end
function __replPrint(object)
    print("=> " .. __replShow(object, 0))
end
|]
