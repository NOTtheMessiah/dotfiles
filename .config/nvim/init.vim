set ruler
set number
filetype plugin indent on
set ignorecase
let $NVIM_TUI_ENABLE_TRUE_COLOR=0
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
set tabstop=4 softtabstop=0 expandtab shiftwidth=4
set clipboard=unnamed
colorscheme gruvbox "jellybeans github
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
call plug#end()

let g:syntastic_python_flake8_args='--ignore=E501,E225'
let g:airline_powerline_fonts = 1
let g:deoplete#enable_at_startup = 1
let g:deoplete#max_menu_width = 40
" deoplete tab-complete
" inoremap <expr><tab> pumvisible() ? "\<C-n>" : "\<Tab>"
let g:vimwiki_list = [{'path':'~/lib/wiki'}]
