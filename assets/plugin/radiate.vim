command! Radiate call radiation#Radiate()

" If we can do it async, then run
" in the backgrout automagically
au BufEnter *.c,*.cpp,*.h,*.hpp call radiation#QuickRadiate()
au BufLeave *.c,*.cpp,*.h,*.hpp call radiation#Kill()
