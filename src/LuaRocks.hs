module LuaRocks where

import System.Environment
import System.Directory
import qualified Data.Text as T
import Data.List
import System.Process

paths :: IO [FilePath]
paths = do
    path <- getEnv "PATH"
    return $ map (T.unpack) $ (T.splitOn (T.pack ":") . T.pack) path

getLuaRocksSandbox :: IO FilePath
getLuaRocksSandbox = do
    home <- getHomeDirectory
    let d = home ++ "/.local/hslua-repl-sandbox"
    createDirectoryIfMissing True d
    return d

removeLuaRocksSandbox :: IO ()
removeLuaRocksSandbox = do
    d <- getLuaRocksSandbox
    --removeDirectoryRecursive d
    putStrLn ("removing" ++ d)

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

getLuaRocks :: IO [FilePath]
getLuaRocks = do
    ps <- paths
    -- make sure that the paths are all directories
    isDir <- sequence $ map doesDirectoryExist ps
    let dirsWithPred = zip ps isDir
    -- all dirs are guarenteed to be directories
    let dirs = map fst (filter snd dirsWithPred)
    files <- sequence $ map listDirectory dirs
    return $ concat $ filter (isSuffixOf "luarocks") <$> files

luaRocksInstall :: String -> IO ()
luaRocksInstall pkgName = do
    lRs <- getLuaRocks
    tree <- getLuaRocksSandbox
    case lRs of
        [] -> do
            putStrLn "luaRocksInstall called while LuaRocks was not loaded, so nothing will happen."
            return ()
        lR:_ -> do
            callProcess lR ["--tree="++tree, "install", pkgName]

luaRocksSearch :: String -> IO ()
luaRocksSearch pkgName = do
    lRs <- getLuaRocks
    case lRs of
        [] -> do
            putStrLn "luaRocksSearch called while LuaRocks was not loaded, so nothing will happen."
            return ()
        lR:_ -> do
            callProcess lR ["search", pkgName]
