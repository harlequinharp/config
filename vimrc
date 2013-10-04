"this is vim, not vi!
set nocompatible

""""""""""""""""""
" NeoBundle Stuff!
""""""""""""""""""

" required for NeoBundle
filetype off
if has('vim_starting')
    set rtp+=~/.vim/bundle/neobundle.vim
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" NeoBundle itself
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'bling/vim-airline'

NeoBundle 'majutsushi/tagbar'

" Unite! united fuzzy ui plugin"
NeoBundle 'Shougo/unite.vim'

" code completion
NeoBundle 'Shougo/neocomplete'

" filer
NeoBundle 'Shougo/vimfiler.vim'

" git integration
NeoBundle 'tpope/vim-fugitive'

" Improved motion
NeoBundle 'Lokaltog/vim-easymotion'
    let g:EasyMotion_leader_key = '<Leader><Leader>'

" Search results counter
NeoBundle 'IndexedSearch'

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

syntax enable

" cursor position in status line
set ruler
" folding
set foldmethod=indent

" move around windows with ctrl+movement keys
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" save file keybinds
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

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

if executable('coffeetags')
  let g:tagbar_type_coffee = {
        \ 'ctagsbin' : 'coffeetags',
        \ 'ctagsargs' : '--include-vars',
        \ 'kinds' : [
        \ 'f:functions',
        \ 'o:object',
        \ ],
        \ 'sro' : ".",
        \ 'kind2scope' : {
        \ 'f' : 'object',
        \ 'o' : 'object',
        \ }
        \ }
endif

autocmd BufRead *.vala,*.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala,*.vapi setfiletype vala

let vala_comment_strings = 1
let vala_space_errors = 1
