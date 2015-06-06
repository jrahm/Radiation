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



function radiation#RadiationAuto()

augroup Radiation
    autocmd!

    " If we can do it async, then run
    " in the backgrout automagically
    autocmd BufEnter *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()
    autocmd BufLeave *.c,*.cpp,*.h,*.hpp call radiation#Kill()
    autocmd CursorHold *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()
    autocmd InsertLeave *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()
    autocmd InsertEnter *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()

augroup END

endfunction

function radiation#RadiationNoAuto()

augroup Radiation
    autocmd!
    autocmd BufEnter *.c,*.cpp,*.h,*.hpp call radiation#TrySource()
    autocmd CursorHold *.c,*.cpp,*.h,*.hpp call radiation#TrySource()
    autocmd InsertLeave *.c,*.cpp,*.h,*.hpp call radiation#TrySource()
    autocmd InsertEnter *.c,*.cpp,*.h,*.hpp call radiation#TrySource()
augroup END

endfunction

function! radiation#Radiate()
    " Call the python function
    " to radiate the file with the filetype
    let g:radiation_running=1
	exec printf('python radiate("%s", "%s")', expand("%"), &filetype)

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
    let g:radiation_running=0
    redraw!
endfunction

function! radiation#TrySource()
    exec printf('python radiation_source("%s")', expand("%"))   
endfunction

let g:radiation_running=0

function! radiation#SourceAndRun()
    if g:radiation_running == 0
        call radiation#Radiate()
    endif
endfunction

call s:Initialize()
