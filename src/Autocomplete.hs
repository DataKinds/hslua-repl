module Autocomplete where

--getNames

autocomplete :: Maybe (String -> IO [String])
autocomplete = Just (\_ -> return ["XD"])
