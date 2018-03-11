module LuaRocks where

import System.Environment
import System.Directory
import qualified Data.Text as T
import Data.List

paths :: IO [FilePath]
paths = do
    path <- getEnv "PATH"
    return $ map (T.unpack) $ (T.splitOn (T.pack ":") . T.pack) path

canAccessLuaRocks :: IO Bool
canAccessLuaRocks = do
    ps <- paths
    -- make sure that the paths are all directories
    isDir <- sequence $ map doesDirectoryExist ps
    let dirsWithPred = zip ps isDir
    let dirsFilteredPred = filter snd dirsWithPred
    -- all dirs are guarenteed to be directories
    let dirs = map fst dirsFilteredPred
    files <- sequence $ map listDirectory dirs
    return $ (any . any) (isInfixOf "luarocks") files
