syntax on                       " enables syntax highlight
set autoindent                  " automatically indent lines
set expandtab                   " converts tabs to spaces
set hidden                      " hides buffer instead of closing
set hlsearch                    " highlight search results
set ignorecase                  " case-insentive search
set incsearch                   " incremental search
set noswapfile                  " we have... git nowadays
set rnu                         " show line numbers
set ruler                       " shows the column
set showcmd                     " shows cmds in the last line of the screen
set tabstop=4                   " number of spaces in a tab
set wrap                        " wrap on words and makes navigation more obvious
set wildmenu                    " tab-completion on commands
set backspace=indent,eol,start  " enable backspace

imap jj <Esc>
map <Space> <Leader>

nnoremap <silent> <Leader>ff :Files<cr>
nnoremap <silent> <Leader>sp :Rg<cr>
nnoremap <silent> <Leader>sf :BLines<cr>

inoremap <silent><expr> <TAB>
			\ pumvisible() ? "\<C-n>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
