    __________             .___.__        __  .__               
    \______   \_____     __| _/|__|____ _/  |_|__| ____   ____  
     |       _/\__  \   / __ | |  \__  \\   __\  |/  _ \ /    \ 
     |    |   \ / __ \_/ /_/ | |  |/ __ \|  | |  (  <_> )   |  \
     |____|_  /(____  /\____ | |__(____  /__| |__|\____/|___|  /
            \/      \/      \/         \/                    \/ 
                                                                                                            
                                            
Radiation is a new Vim plugin in the  early stages of deelopment.
Originally implemented in Ruby then C, after complete refactoring of the
architecture, the current implementation is written in Haskell and is far more
robust than ever before.

## Screenshot
![Screenshot 1](https://raw.github.com/jrahm/Radiation/master/snapshot.jpg)

## What is Radiation?

Radiation is a plugin that gives Vim active and dynamic syntax highlighting to
C++, C, and JavaScript file types. This allows the syntax of each source file
to change with the content of that file.

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

3. Radiation is (mostly) seamless
    * A lot of work has gone into architecting the system to be as seamless as
      possible. This means that you can go on editing your file and radiation
      will happily crunch away and get back when it's done. This means limited
      differences to the user (aside from the awesome new highlighting!).

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
