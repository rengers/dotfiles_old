
fun! MySys()
   return "linux"
endfun
set runtimepath=~/.vim_runtime,~/.vim_runtime/after,\$VIMRUNTIME
source ~/.vim_runtime/vimrc
helptags ~/.vim_runtime/doc

"Latex compile and display set to tt
map tt :w<CR>:! cd '%:p:h';  pdflatex '%:t'; evince '%:t:r.pdf' &<CR><CR>
