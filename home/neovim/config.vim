" show numbers
set number 

" show relative numbers
set relativenumber 

" color scheme
colorscheme noctu

" space as leader key
nnoremap <space> <nop>
let mapleader=' ' 

" escape key
inoremap jk <Esc>
" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

let g:ale_completion_enabled = 1
let g:ale_linters = {'c': ['clang'], 'rust': ['analyzer', 'cargo']}
let g:ale_fixers = {'rust': ['rustfmt'], 'sql': ['pgformatter'], 'nix': ['nixpkgs-fmt'], 'json': ['jq']}
let g:ale_rust_analyzer_config = {'checkOnSave': {'command': 'clippy', 'enable': v:true}}
