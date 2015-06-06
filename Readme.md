	,-.----.                                                   ___                                  
	\    /  \                    ,---,  ,--,                 ,--.'|_    ,--,                        
	;   :    \                 ,---.'|,--.'|                 |  | :,' ,--.'|     ,---.        ,---, 
	|   | .\ :                 |   | :|  |,                  :  : ' : |  |,     '   ,'\   ,-+-. /  |
	.   : |: |   ,--.--.       |   | |`--'_       ,--.--.  .;__,'  /  `--'_    /   /   | ,--.'|'   |
	|   |  \ :  /       \    ,--.__| |,' ,'|     /       \ |  |   |   ,' ,'|  .   ; ,. :|   |  ,"' |
	|   : .  / .--.  .-. |  /   ,'   |'  | |    .--.  .-. |:__,'| :   '  | |  '   | |: :|   | /  | |
	;   | |  \  \__\/: . . .   '  /  ||  | :     \__\/: . .  '  : |__ |  | :  '   | .; :|   | |  | |
	|   | ;\  \ ," .--.; | '   ; |:  |'  : |__   ," .--.; |  |  | '.'|'  : |__|   :    ||   | |  |/ 
	:   ' | \.'/  /  ,.  | |   | '/  '|  | '.'| /  /  ,.  |  ;  :    ;|  | '.'|\   \  / |   | |--'  
	:   : :-' ;  :   .'   \|   :    :|;  :    ;;  :   .'   \ |  ,   / ;  :    ; `----'  |   |/      
	|   |.'   |  ,     .-./ \   \  /  |  ,   / |  ,     .-./  ---`-'  |  ,   /          '---'       
	`---'      `--`---'      `----'    ---`-'   `--`---'               ---`-'                       
                                                                                                 
                                                                                                            
                                                                                                            
                                            
Radiation is a new Vim plugin in the (very) early stages of deelopment.
Originally implemented in Ruby then C, after complete refactoring of the
architecture, the current implementation is written in Haskell and is far more
robust than ever before.

## Screenshot
![Screenshot 1](https://raw.github.com/jrahm/Radiation/master/snapshot.jpg)

## What is Radiation?

Radiation is a plugin that enables context aware syntax highlighting for C++
files (Soon to be extended). This extension tells Vim about classes, structs,
typedefs and namespaces are included so they may be highlighted.

## Why use Radiation?

1. Radiation is fast.
    * I have tried other plugins for this same problem, but all were too slow for me to
    use in even a moderately sized code baes stemming from the fact that many of these
    plugins were implemented in straight VimL or some other scripting language too slow
    to handle the code.

    * Since the guts of Radiation is implemented as a standalone executable and
    is implemented in Haskell, which can take advantage of true multithreadding
    to boost performance. Also, as a stand alone executable if vim is running under
    the `--servername` flag, it will operate asynchonously, leaving the user to
    continue to use Vim while it calculates the extra syntax highlighting.

2. Radiation is not restricted.
    * Since radiation is not tied to the Vim runtime and is implemented in Haskell,
    there are no restrictions at the language level. THe binary may take full advantage
    of multi threadding and may also be updated on the fly. No restart required.

## Why not use Radiation
    
1. Radiation is now a separate binary from Vim and is implemented in a safe
language, so segfaults are no longer an issue. However, stability and potential
deadlock has been known to occur in some cases. Killing the process is sufficient
to halt the deadlock. It is important to be sure of stability before adding
Radiation to any auto commands.

3. If you are running a restricted version of Vim. The plugin requires an unrestricted
version of vim to run since it executes an external program.

## How does it work

1. To install, dowload this repository and run the install script. `./install.sh`
2. Once installed, this plugin will allow you to run the command `:Radiate`. This
will add additional synatax highlighting to C++ and C files.

## Changes

1. Radiation can now compile for Windows! Still in early stages of testing though.
2. Radiation now uses user-private directories in temporary folders to place files
3. Radiation now uses a caching system to radiate files instantly that were previously radiated
4. Radiation system has been completely overhauled to provide the illusion of asychronisity even
   without the server feature!
