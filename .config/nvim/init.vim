set ruler
set number
filetype plugin indent on
set ignorecase
let $NVIM_TUI_ENABLE_TRUE_COLOR=0
"let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
set tabstop=4 softtabstop=0 expandtab shiftwidth=4
set clipboard=unnamed
set mouse=a
colorscheme gruvbox "jellybeans github
"let g:terminal_color_0  = '#3e4a47'
"let g:terminal_color_8  = '#67746d'
"let g:terminal_color_7  = '#94a194'
"let g:terminal_color_15 = '#c6cfbe'
"let g:terminal_color_1  = '#c27157'
"let g:terminal_color_3  = '#b09440'
"let g:terminal_color_2  = '#5e944c'
"let g:terminal_color_6  = '#1ca790'
"let g:terminal_color_4  = '#549ca1'
"let g:terminal_color_5  = '#bb8699'
"let g:terminal_color_9  = '#e9ac9c'
"let g:terminal_color_11 = '#f6c384'
"let g:terminal_color_10 = '#bac172'
"let g:terminal_color_14 = '#99daa2'
"let g:terminal_color_12 = '#86c6b9'
"let g:terminal_color_13 = '#cfc8cb'
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_contrast_light = 'soft'
set background=dark
syntax on
" set cursorline
" set cursorcolumn
autocmd InsertEnter * :set number
autocmd InsertLeave * :set relativenumber

" Commenting blocks of code.
autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
autocmd FileType sh,ruby,python   let b:comment_leader = '# '
autocmd FileType conf,fstab       let b:comment_leader = '# '
autocmd FileType tex              let b:comment_leader = '% '
autocmd FileType mail             let b:comment_leader = '> '
autocmd FileType vim              let b:comment_leader = '" '
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>

vmap <C-C> "+y
noremap ,s <Esc>:terminal<CR>
noremap ,e <Esc>:Explore<CR>
imap <C-Return> <CR><CR><C-o>k<Tab>
" Tab Management
noremap ,t <Esc>:tabnew<CR>
noremap ,w <Esc>:tabclose<CR>
noremap ,oxm <Esc>:e ~/.xmonad/xmonad.hs<CR>
noremap ,onv <Esc>:e ~/.config/nvim/init.vim<CR>
nnoremap <C-Insert> :tabnew<CR>
nnoremap <C-Delete> :tabclose<CR>
tnoremap <Esc> <C-\><C-n>

function! DoRemote(arg)
  UpdateRemotePlugins
endfunction

set rtp^=/usr/share/vim/vimfiles/

call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'ap/vim-css-color'
Plug 'tomvanderlee/vim-kerboscript'
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'zchee/deoplete-jedi'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'eagletmt/neco-ghc'
Plug 'Shougo/neco-vim'
Plug 'JuliaLang/julia-vim'
Plug 'cespare/vim-toml'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'benekastah/neomake'
Plug 'derekelkins/agda-vim'
Plug 'vimwiki/vimwiki'
Plug 'ervandew/supertab'
Plug 'tpope/vim-surround'
call plug#end()

let g:syntastic_python_flake8_args='--ignore=E501,E225'
let g:airline_powerline_fonts = 0
let g:deoplete#enable_at_startup = 1
let g:deoplete#max_menu_width = 40
" deoplete tab-complete
" inoremap <expr><tab> pumvisible() ? "\<C-n>" : "\<Tab>"
let g:vimwiki_list = [{'path':'~/lib/wiki'}]
let mapleader=' '
let maplocalleader='\\'
"let g:agda_extraincpaths = ["/usr/local/Cellar/agda/2.5.2/lib/agda/src/"]
