"this is vim, not vi!
set nocompatible 

""""""""""""""""""
" NeoBundle Stuff!
""""""""""""""""""

" required for NeoBundle
filetype off
if has('vim_starting')
    set rtp+=~/.vim/bundle/neobundle.vim
    set rtp+=~/.local/lib/python3.3/site-packages/powerline/bindings/vim
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" NeoBundle itself
NeoBundleFetch 'Shougo/neobundle.vim'

" Improved motion 
NeoBundle 'Lokaltog/vim-easymotion'
    let g:EasyMotion_leader_key = '<Leader><Leader>'

" Search results counter
NeoBundle 'IndexedSearch'

" solarized for vim
NeoBundle 'altercation/vim-colors-solarized'
    set t_Co=16
    let g:solarized_termcolors=16
    if has('gui_running')
        set guifont=DejaVu\ Sans\ Mono\ 9
    endif

NeoBundle 'kchmck/vim-coffee-script'

" undoing above required for NeoBundle, tells vim to load 
" filetype specific plugin and indent files
filetype plugin indent on

"""""""""""""""""""""
" end NeoBundle stuff
"""""""""""""""""""""

" I like modelines! :)
set modeline

" tabs and spaces handling
set et ts=4 sts=4 sw=4
autocmd FileType html setlocal ts=2 sts=2 sw=2

" always show status bar
set ls=2

" incremental search
set incsearch

" line numbers
set nu

" autocompletion of files and commands behaves like shell
" (complete only the common part, list the options that match)
set wildmode=list:longest

set clipboard="unnamedplus,exclude:cons\|linux"

set ve=block
set go="aceimtT"

" to use fancy symbols for powerline, uncomment the following line and use a
" patched font (more info on the README.rst)
let g:Powerline_symbols = 'fancy'

syntax on
set background=dark
colorscheme solarized

" cursor position in status line
set ruler
" folding
set foldmethod=indent
autocmd FileType c cpp setlocal foldmethod=syntax

" move around windows with ctrl+movement keys
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>

" helper for above
function! VisualSelection(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" Return to last edit position when opening files 
autocmd BufReadPost *
	\ if line("'\"") > 0 && line("'\"") <= line("$") |
	\   exe "normal! g`\"" |
	\ endif
" Remember info about open buffers on close
set viminfo^=%

" Always show the status line
set laststatus=2

" function to auto-insert include guards in C/C++ headers
function! s:insert_gates()
  let gatename = substitute(toupper(expand("%:t")), "\\.", "_", "g")
  execute "normal! i#ifndef " . gatename
  execute "normal! o#define " . gatename . " "
  execute "normal! Go#endif /* " . gatename . " */"
  normal! kk
endfunction

autocmd BufNewFile *.{h,hpp} call <SID>insert_gates()
