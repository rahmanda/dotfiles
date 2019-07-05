set nocompatible
" turn off beeping everywhere
set visualbell
set t_Co=256
set laststatus=2
syntax enable
set background=dark
set number
filetype off
set wildmenu
set rtp+=~/.vim/bundle/Vundle.vim

" vundle begin
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
" i don't know what this is use for, but it is needed for dependencies
Plugin 'L9'
Plugin 'junegunn/fzf.vim'
" mainstream tree explorer
Plugin 'scrooloose/nerdtree'
" syntax checker that runs after you save your buffer
Plugin 'scrooloose/syntastic'
" editor configurator
Plugin 'editorconfig/editorconfig-vim'
" status line
Plugin 'itchyny/lightline.vim'
" line up text
Plugin 'godlygeek/tabular'
" syntax highlighter for scss
Plugin 'cakebaker/scss-syntax.vim'
" syntax highlighter for javascript
Plugin 'pangloss/vim-javascript'
" syntax highlighter for markdown
Plugin 'plasticboy/vim-markdown'

call vundle#end()    
filetype plugin indent on 
" vundle end

" syntastic configuration
let g:syntastic_javascript_checkers = ['jshint']
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" vim-markdown configuration
let g:vim_markdown_folding_disabled = 1
