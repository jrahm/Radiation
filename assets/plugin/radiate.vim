" quick common to radiate
command! Radiate call radiation#Radiate()

function TryHighlight(name, cterm)
    if ! hlexists(a:name)
        exe 'highlight '.a:name.' ctermfg='.a:cterm
    endif
endfunction

" If we can do it async, then run
" in the backgrout automagically
au BufEnter *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()
au BufLeave *.c,*.cpp,*.h,*.hpp call radiation#Kill()
au CursorHold *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()
au InsertLeave *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()
au InsertEnter *.c,*.cpp,*.h,*.hpp call radiation#SourceAndRun()

let s:radiation=expand("<sfile>:p:h")."/radiation" " Path to the current directory

if executable(s:radiation) 
    let g:radiation_binary=s:radiation
endif

" Change these if you want!
call TryHighlight("RadiationCFunction", "5")
call TryHighlight("RadiationCStruct",   "3")
call TryHighlight("RadiationCEnum",     "3")
call TryHighlight("RadiationCTypedef",  "3")
call TryHighlight("RadiationCppClass",  "3")
