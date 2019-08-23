syntax on
set t_Co=256
" turn off beeping everywhere
set visualbell
set nocompatible
set laststatus=2
set number
filetype off
set wildmenu
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.fzf

" vundle begin
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
" i don't know what this is use for, but it is needed for dependencies
Plugin 'L9'
" fuzzy search files and search word on directory
Plugin 'junegunn/fzf.vim'
" color scheme's collections
Plugin 'flazz/vim-colorschemes'
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
" move line
Plugin 'matze/vim-move'
" syntax highlighter for vue
Plugin 'posva/vim-vue'
" git client
Plugin 'tpope/vim-fugitive'

call vundle#end()    
filetype plugin indent on 
" vundle end

colorscheme monokain

" syntastic configuration
let g:syntastic_javascript_checkers = ['eslint']
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" vim-markdown configuration
let g:vim_markdown_folding_disabled = 1

" vim move configuration
let g:move_key_modifier = 'C'

