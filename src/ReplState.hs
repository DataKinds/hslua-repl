module ReplState where
    
data ReplState = ReplState {
    replPrompt :: String,
    loadedFiles :: [String]
}
defaultReplState :: ReplState
defaultReplState = ReplState { replPrompt = "Lua$ ", loadedFiles = [] }
updateReplPrompt :: String -> ReplState -> ReplState
updateReplPrompt p rS = rS { replPrompt = p }
updateLoadedFiles :: String -> ReplState -> ReplState
updateLoadedFiles fh rS = rS { loadedFiles = fh:(loadedFiles rS) }
