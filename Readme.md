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
