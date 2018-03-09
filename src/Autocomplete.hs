module Autocomplete where

--getNames :: Lua [String]
--getNames = do
--    getglobal "__replGlobalNames()"


autocomplete :: Maybe (String -> IO [String])
autocomplete = Just (\_ -> return ["XD"])
