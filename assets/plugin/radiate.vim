" quick common to radiate
command! Radiate call radiation#Radiate()
command! RadiateAuto call radiation#RadiationAuto()
command! RadiateNoAuto call radiation#RadiationNoAuto()

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

augroup Radiation
    autocmd BufEnter    * call radiation#SourceAndRun()
    autocmd BufLeave    * call radiation#Kill()
    autocmd CursorHold  * call radiation#SourceAndRun()
    autocmd InsertLeave * call radiation#SourceAndRun()
    autocmd InsertEnter * call radiation#SourceAndRun()
augroup END
