### Mac OSX installation:

#### Step 1: Install Stack. Stack is a cross-platform program for developing Haskell projects 
##### Mac OSX
(a) Install homebrew. See https://brew.sh for details
```
> /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
```
(b) Install the Haskell tool stack
```
> brew install stack
```

##### Linux

(a) Download stack
```
curl -sSL https://get.haskellstack.org/ | sh
```

(b) Add path
```
export PATH="/home/artem/.local/bin:$PATH"

```

#### Step 3: Test GHC’s interactive environment
Open console and run 
```
> stack exec – ghci
```
Type the following command and press enter
```
putStrLn "Hello, Haskell!"
```
Exit by typing `:q`

#### Step 4: Test Glasgow Haskell Compiler (ghc)
Create a file by typing
```
> nano helloworld.hs
```
Input and save the following code
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
> git clone https://github.com/haskell/haskell-ide-engine --recursive 
> cd haskell-ide-engine
> stack ./install.hs hie
> stack ./install.hs hie
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

#### Step 8: Install Haskell extension in VSCode: 
Run VSCode and open Code -> Preferences -> Extension. Type “Haskell Language Server” in the Search field. Click “install”. Restart VSCode.

#### Step 9: Install Haskell debugger:
Run VSCode and open Code -> Preferences -> Extension. Type "phoityne-vscode" in the Search field. Click “install”.
Open a terminal and run
```
stack install haskell-dap ghci-dap haskell-debug-adapter
```
#### Step 10: Install
Run VSCode and open Code -> Preferences -> Extension. Type "haskell-linter" in the Search field. Click “install”.


### References

#### Why to study Haskell
[1] https://serokell.io/blog/10-reasons-to-use-haskell
[2] https://www.fpcomplete.com/blog/2018-haskell-survey-results/

#### IDES
[1] https://github.com/haskell/haskell-ide-engine#installation
[2] https://www.fpcomplete.com/blog/mainstream-ides-haskell/

#### GUI
[1] HsQML then allows you to bind together front-end designs written in QML with back-end logic written in Haskell to create complete applications using the strengths of both. https://www.gekkou.co.uk/software/hsqml/
[2]  https://wiki.haskell.org/Gtk2Hs

#### Cool examples
[1] Gaussian smoothing implemented in Haskell and compared to Python. https://gist.github.com/maksbotan/03d34a4a463b6fe91093e7f85d90a9b4#file-1_text-md
[2] Generating fractal set

#### Books or tutorials
[1] http://book.realworldhaskell.org
[2] https://www.seas.upenn.edu/~cis194/spring13/lectures.html
[3] https://www.fpcomplete.com/blog/
[4] https://www.fpcomplete.com/haskell/tutorial/stack-script/
