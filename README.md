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

#### Step 8: Run VSCode and open  Code -> Preferences -> Extension. Type “Haskell Language Server” in the Search field. Click “install”. Restart VSCode.


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
