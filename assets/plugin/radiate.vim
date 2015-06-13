" quick common to radiate
command! Radiate call radiation#Radiate()
command! Radiat  call radiation#Radiate()

command! RadiationAuto call RadiationAuto()
command! RadiationNoAuto call RadiationNoAuto()
command! RadiationSource call radiation#TrySource()

command! RadiationOpen call radiation#Open()
command! RadiationDelete call radiation#Remove()
command! RadiationClean call radiation#Clean() 

command! RadiationOpenLog call radiation#OpenLog()

function TryHighlight(name, cterm)
    if ! hlexists(a:name)
        exe 'highlight '.a:name.' ctermfg='.a:cterm
    endif
endfunction

if has("win32")
    let s:radiation=expand("<sfile>:p:h")."/radiation.exe" " Path to the current directory
else
    let s:radiation=expand("<sfile>:p:h")."/radiation" " Path to the current directory
endif

if executable(s:radiation) 
    let g:radiation_binary=s:radiation
endif

" Change these if you want!
call TryHighlight("RadiationCFunction", "5")
call TryHighlight("RadiationCStruct",   "3")
call TryHighlight("RadiationCEnum",     "3")
call TryHighlight("RadiationCTypedef",  "3")
call TryHighlight("RadiationCppClass",  "3")

" This function sets up radiation to automatically run when
" many different events happen. This should only really be used
" if radiation is set to not block. This should not be a problem
" with the new architecture of the code.
function RadiationAuto()

augroup Radiation
    " clear everything out
    autocmd!

    " If we can do it async, then run
    " in the backgrout automagically
    autocmd BufEnter    * call radiation#SourceAndRun()
    autocmd BufWrite    * call radiation#SourceAndRun()
    autocmd BufLeave    * call radiation#Kill()
    autocmd CursorHold  * call radiation#SourceAndRun()
    autocmd InsertLeave * call radiation#SourceAndRun()
    autocmd InsertEnter * call radiation#SourceAndRun()

augroup END

endfunction

" The opposite of the above. Radiation will still try to source
" the file if it can, but no new instances of radiation will be
" spawned unless the user uses the Radiation command.
function RadiationNoAuto()

augroup Radiation
    autocmd!
    autocmd BufEnter    * call radiation#TrySource()
    autocmd CursorHold  * call radiation#TrySource()
    autocmd InsertLeave * call radiation#TrySource()
    autocmd InsertEnter * call radiation#TrySource()
augroup END

endfunction

call RadiationAuto()
