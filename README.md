# hslua-repl
A fully featured Lua REPL written in Haskell

## Usage
```
git clone https://github.com/aearnus/hslua-repl
cd hslua-repl/
stack build
stack exec hslua-repl-exe
```

## Built-in Help Reference
```
hslua-repl v. 1.0.0
Copyright 2018 Aearnus
Uses the `hslua` Haskell library to interact with Lua 5.3.
Available commands:
    :quit        --   Exits the interpreter.
    :prompt      --   Sets the interpreter prompt.
    :load        --   Load a Lua file from the current directory.
    :help        --   Prints this text.
```
