let s:script_dir = expand('<sfile>:p:h')
let s:radiation_initialized = 0
let g:radiation_log_level = "error"

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

function! radiation#Radiate()
    " Call the python function
    " to radiate the file with the filetype
	exec printf('python radiate( "%s", "%s", "%s" )', expand("%"), &filetype, "" )

    redraw!

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
    if exists('v:servername') && v:servername != ""
        let filename = '/tmp/radiationx.'.v:servername.'.vim'
        if filereadable(filename)
            " check to see if the file to source exists.
            " source and delete
            exec 'source '.filename
            call delete(filename)
        endif
    endif
endfunction

call s:Initialize()
