### Mac OSX and Linux installation steps:

#### Step 1: Install Stack. 
Stack is a cross-platform program for developing Haskell projects and managing their dependencies.

* ##### Mac OSX
(a) Install homebrew. See https://brew.sh for details
```
> /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
```
(b) Install the Haskell tool Stack
```
> brew install stack
```

* ##### Linux

(a) Download Stack
```
curl -sSL https://get.haskellstack.org/ | sh
```

(b) Update your PATH enviroment variable by concatenating the exisiting PATH as follows
```
export PATH="${HOME}/.local/bin:$PATH"
```
If you use Z shell as a command interpreter for shell scripting, then the above line should be added to  
```
~/.zshrc
```


#### Step 2: Test GHC’s (The Glasgow Haskell Compiler) interactive environment
Open console and run 
```
> stack ghci
```
Type the following command and press enter
```
putStrLn "Hello, Haskell!"
```
Exit by typing `:q`

#### Step 3: Test Glasgow Haskell Compiler (ghc)
Create a file by typing
```
> nano helloworld.hs
```
where nano is a text editor, any other editor of you choice will work too.
Type and save the following code
```
module Main where  
main :: IO () 
main = putStrLn "Hello from Haskell!"  
```

Build the program by
```
> stack exec -- ghc helloworld -o helloworld
```

Now test the program by running it
```
> ./helloworld
```

#### Step 4: Download VSCode
Visual Studio Code is a free source-code editor made by Microsoft for Windows, Linux and macOS. Features include support for debugging, syntax highlighting, intelligent code completion, snippets, code refactoring, and embedded Git.
https://code.visualstudio.com
You could also use an OS  software manager to download and install VSCode

#### Step 5: Install VS Code extensions

* ##### MacOSX
```
> git clone https://github.com/haskell/haskell-ide-engine --recursive 
> cd haskell-ide-engine
> stack ./install.hs hie
> stack ./install.hs hie
```

* ##### Linux: Debian 9/Ubuntu 18.04 or earlier
```
> sudo apt install libicu-dev libtinfo-dev libgmp-dev
> git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
> cd haskell-ide-engine
> stack ./install.hs hie
```

* ##### Linux: Debian 10/Ubuntu 18.10 or later
```
> sudo apt install libicu-dev libncurses-dev libgmp-dev # also zlib1g-dev if not installed
> git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
> cd haskell-ide-engine
> stack ./install.hs hie
```

#### Step 6: Install Haskell extension in VSCode: 
Run VSCode, open Code -> Preferences -> Extension. Type “Haskell Language Server” in the Search field. Click “install”. Restart VSCode.

#### Step 7: Install Haskell debugger:
Run VSCode and open Code -> Preferences -> Extension. Type "phoityne-vscode" in the Search field. Click “install”.
Open a terminal and run
```
stack install haskell-dap ghci-dap haskell-debug-adapter
```
#### Step 8: Create a new project and test the debugger (https://github.com/phoityne/hdx4vsc)
1. Open a terminal within VSCode: Terminal -> New Terminal
2. Check that gchi-gap and haskell-debug-adapter are visible to VSCode by running 
```
ghci-dap --help
```
```
haskell-debug-adapter --version
```
3. Create a project named sample by typing within the openned above terminal 
```
stack new sample --bare
```
4. Use VSCode menu to open the folder with the project.
5. Press F7 to build the project, and then press F8 to run the tests. 
6. Click on the icon on the left-hand side, that depicts a play button (a triangle) and a small bug on it. Then click "create a launch.json file". In the popped up  menu, choose "haskell-debug-adapter". This will setup a build configuration.
7. Open Main.hs and press F10, this should run the code.
8. To set a breakpoint, click to the left of a line number (in the source file e.g. Main.hs) on a dark red dot, a red dot should appear. Press F10 to run the code. Now the executation will stop at the breakpoint. 

![Debugger installation](https://raw.githubusercontent.com/phoityne/hdx4vsc/master/docs/08_quickstart.gif)




### References

#### Why to study Haskell
1. https://serokell.io/blog/10-reasons-to-use-haskell
2. https://www.fpcomplete.com/blog/2018-haskell-survey-results/

#### IDES
1. https://github.com/haskell/haskell-ide-engine#installation
2. https://www.fpcomplete.com/blog/mainstream-ides-haskell/

#### GUI
1. HsQML then allows you to bind together front-end designs written in QML with back-end logic written in Haskell to create complete applications using the strengths of both. https://www.gekkou.co.uk/software/hsqml/
2.  https://wiki.haskell.org/Gtk2Hs

#### Cool examples
1. Gaussian smoothing implemented in Haskell and compared to Python. https://gist.github.com/maksbotan/03d34a4a463b6fe91093e7f85d90a9b4#file-1_text-md
2. Generating fractal set

#### Books or tutorials
1. http://book.realworldhaskell.org
2. https://www.seas.upenn.edu/~cis194/spring13/lectures.html
3. https://www.fpcomplete.com/blog/
4. https://www.fpcomplete.com/haskell/tutorial/stack-script/
