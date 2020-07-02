### Mac OSX installation:

#### Step 1: Install homebrew. See https://brew.sh for details
```
> /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
```
#### Step 2: Install the Haskell tool stack
```
> brew install stack
```

#### Step 3: Test GHC’s interactive environment
Open console and run 
```
> stack exec – ghci
```
type the following command and press enter
```
putStrLn "Hello, Haskell!"
```
exit by typing `:q`

#### Step 4: Test Glasgow Haskell Compiler (ghc)
Create a file by typing
```
> nano helloworld.hs
```
and then type and save the following code
```
module Main where  
main :: IO () 
main = putStrLn "Hello from Haskell!"  
```

Build the program by
```
> stack exec -- ghc helloworld -o helloworld
```
Now test the program by running
```
> ./helloworld
```

#### step 5: Download VSCode
https://code.visualstudio.com

#### Step 6: Install VS Code extensions
```
> git clone https://github.com/haskell/haskell-ide-engine
> cd haskell-ide-engine
> stack install
```

#### Step 7: Make sure to add the installation path to the PATH environment variable. 
If you use Z shell as a command interpreter for shell scripting, then add 
```
PATH=${HOME}/.local/bin:$PATH
```
to 
```
~/.zshrc
```

#### Step 8: Run VSCode and open  Code -> Preferences -> Extension. Type “Haskell Language Server” in the Search field. Click “install”. Restart VSCode.
