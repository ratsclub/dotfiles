" show numbers
set nu

" show relative numbers
set rnu

" color scheme
colorscheme noctu

nnoremap <space> <nop>
let mapleader=" "

" escape key
inoremap jk <Esc>

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

