# hslua-repl
A fully featured Lua REPL written in Haskell

## Usage
```
git clone https://github.com/aearnus/hslua-repl
cd hslua-repl/
stack build
stack exec repl
```

## Built-in Help Reference
```
Lua$ :help
hslua-repl v. 1.0.0
Copyright 2018 Aearnus
Uses the `hslua` Haskell library to interact with Lua 5.3.
Type `:help` to get help.
Is LuaRocks loaded? True.

Available commands:
  :quit                 --   Exits the interpreter.
  :prompt <string>      --   Sets the interpreter prompt.
  :load <file path>     --   Load a Lua file from the current directory.
  :reload               --   Reloads the currently loaded Lua files.
  :globals              --   Prints a list of the currently loaded globals (from _G).
  :lr <command>         --   Interact with LuaRocks if it is loaded. Type `:lr help` for more.
  :help                 --   Prints this text.
```

## LuaRocks Help Reference
```
Lua$ :lr
Is LuaRocks loaded? True.
Available LuaRocks commands:
  :lr help                 --   Print this text.
  :lr install <rock name>  --   Install a rock from LuaRocks.
```
