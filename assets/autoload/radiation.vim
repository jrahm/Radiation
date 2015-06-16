
let s:script_dir = expand('<sfile>:p:h')
let g:radiation_script_directory = s:script_dir
let s:radiation_initialized = 0
let g:radiation_log_level = "error"

" initializes the radiation script. This sets up variables
" needed to run Radiation, as well as initializes the python
" script.
function! s:Initialize()

    if s:radiation_initialized == 0
        " The script directory with the python file
	    let s:radiation_python_dir = s:script_dir . '/script/'
    
	    " Call the python file first of all to load the function definitions
	    exec 'pyfile ' . s:radiation_python_dir . 'radiation.py'
    
        " So we do not have to call this again
        let s:radiation_initialized = 1
    endif

    
endfunction


" This function sets up radiation to automatically run when
" many different events happen. This should only really be used
" if radiation is set to not block. This should not be a problem
" with the new architecture of the code.
function radiation#RadiationAuto()

augroup Radiation
    " clear everything out
    autocmd!

    " If we can do it async, then run
    " in the backgrout automagically
    autocmd BufEnter    * call radiation#SourceAndRun()
    autocmd BufLeave    * call radiation#Kill()
    autocmd CursorHold  * call radiation#SourceAndRun()
    autocmd InsertLeave * call radiation#SourceAndRun()
    autocmd InsertEnter * call radiation#SourceAndRun()

augroup END

endfunction

" The opposite of the above. Radiation will still try to source
" the file if it can, but no new instances of radiation will be
" spawned unless the user uses the Radiation command.
function radiation#RadiationNoAuto()

augroup Radiation
    autocmd!
    autocmd BufEnter    * call radiation#TrySource()
    autocmd CursorHold  * call radiation#TrySource()
    autocmd InsertLeave * call radiation#TrySource()
    autocmd InsertEnter * call radiation#TrySource()
augroup END

endfunction

" Invoke Radiation! Calls into the python which will spawn the haskell
" code.
function! radiation#Radiate()
    " Call the python function
    " to radiate the file with the filetype
	exec printf('python radiate("%s")', &filetype)

endfunction

function! radiation#QuickRadiate()
    if exists('v:servername') && v:servername != ""
        call radiation#Radiate()
    endif
endfunction

" Kill the currently running Radiation process.
" This is useful for cases where 
function! radiation#Kill()
    python kill_running()
    redraw!
endfunction

function! radiation#TrySource()
    exec 'python radiation_source()'
endfunction

function! radiation#SourceAndRun()
    call radiation#TrySource()
    call radiation#Radiate()
endfunction

function! radiation#Open()
    python radiation_open_vimfile()
endfunction

function! radiation#Remove()
    python radiation_remove_synfile()
endfunction

function! radiation#Clean()
    python radiation_clean()
endfunction

function! radiation#OpenLog()
    python radiation_open_log()
endfunction

function! radiation#RadiateRecursive(ftype)
    python radiate_all()
endfunction

call s:Initialize()
