
fun! MySys()
   return "linux"
endfun
set runtimepath=~/.vim,~/.vim/after,\$VIMRUNTIME
source ~/.vim/vimrc
helptags ~/.vim/doc

"Latex compile and display set to tt
map tt :w<CR>:! cd '%:p:h';  pdflatex '%:t'; evince '%:t:r.pdf' &<CR><CR>
