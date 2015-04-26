execute pathogen#infect()
set encoding=utf-8

set noerrorbells visualbell t_vb=

" Tabs and Indentations
set ts=2 sw=2 sts=2 expandtab
set number

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

silent execute '!mkdir ~/.vimtmp 2>/dev/null'
set backupdir=~/.vimtmp//
set directory=~/.vimtmp//
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

set t_Co=256
syntax on
filetype on
set hlsearch
set background=dark

if has("autocmd")

    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on

    " For all text files set 'textwidth' to 78 characters.
    autocmd FileType text setlocal textwidth=78

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    " Also don't do it when the mark is in the first line, that is the default
    " position when opening a file.
    autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

    autocmd vimenter * if !argc() | NERDTree | endif
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
    
    au BufRead,BufNewFile *.sc set filetype=scala
    au BufRead,BufNewFile *.json set filetype=json

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

let g:vim_json_syntax_conceal = 0
let b:did_llpplugin = 1

color xoria256

