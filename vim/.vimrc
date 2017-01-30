 " -------Preamble Stuff---------
 "
 " ============================================
 "                 Christopher Findeisen
 "                 Secret Sauce
 "                   "(>^.^<)"
 " ============================================
 "
 " -------Obvious Stuff-------
   set nocompatible
 " set UNICODE so we can use special characters
   set fileencoding=utf-8
   inoremap fd <Esc>
   imap :w <Esc>:w<Enter>
   set backspace=2
   set pastetoggle=<F10>
   inoremap <C-v> <F10><C-r>+<F10>
   set clipboard=unnamed
   set modelines=0
   set showcmd
   vnoremap <C-c> "*y let mapleader = ","
   let g:mapleader = ","
   set autoread
   set autochdir
   set scrolloff=3


   set wildmenu
   set ruler
   set laststatus=2
   noremap <Leader>r :call NumberToggle()<cr>
   nnoremap  <Leader>f :nohl<CR>
   inoremap {<cr> {<cr>}<c-o>O<Tab>


 " Real programmers don't use TABs but spaces
   set tabstop=4
" converts tabs
   set softtabstop=4
" auto-indent width
   set shiftwidth=2
   set shiftround
   set expandtab
   set smartindent

 " Search Stuff
   set hlsearch
   set incsearch
   set ignorecase
   set smartcase

 " Dumbbbbshit errros -_-
   set nobomb
 " Don't redraw while executing macros (good performance config)
   set lazyredraw
 " Moving around in really long lines
   nnoremap j gj
   nnoremap k gk

  vnoremap j gj
  vnoremap k gk
  nnoremap gj j
  nnoremap gk k
  "------Mouse Stuff-------
" Mouse working like normal
  set mouse=a" on OSX press ALT and click
" TODO: Make this work even with TMUX, Terminal, etc
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
elseif $TERM_PROGRAM =~ "iTerm"
  let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
  let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
"------LightLine--------
let g:lightline = {
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ], [ 'bufferline' ] ],
  \ },
  \ 'component': {
  \   'bufferline': '%{bufferline#refresh_status()}%{g:bufferline_status_info.before . g:bufferline_status_info.current . g:bufferline_status_info.after}'
  \ },
\   'bufferline': '%{bufferline#refresh_status()}%{g:bufferline_status_info.before}%#TabLineSel#%{g:bufferline_status_info.current}%#LightLineLeft_active_3#%{g:bufferline_status_info.after}'
  \ }

"------Syntastic--------
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0

"------Buffers > Tabs-----
" To open a new empty buffer
" This replaces :tabnew which I used to bind to this mapping
nnoremap <leader>T :enew<cr>

" Move to the next buffer
nnoremap <leader>l :bnext<CR>

" Move to the previous buffer
nnoremap <leader>h :bprevious<CR>

" Close the current buffer and move to the previous one
" This replicates the idea of closing a tab
nnoremap <leader>bq :bp <BAR> bd #<CR>

" Show all open buffers and their status
nnoremap <leader>bl :ls<CR>
set hidden

"------Window/Screen------
"
" Always show line numbers, but only in current window.
" TEMP REMOVAL. Fucking with tmux
set number
":au WinEnter * :setlocal number
":au WinLeave * :setlocal nonumber

"if something

" Automatically resize vertical splits.
:au WinEnter * :redraw!
"-------------Folding-------------
set foldlevelstart=0

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" "Refocus" folds
nnoremap ,z zMzvzz


:augroup retain_folding
autocmd BufWinLeave *.* mkview!
autocmd BufWinEnter *.* silent loadview
:augroup END
" Make zO recursively open whatever top level fold we're in, no matter where the
" cursor happens to be.
nnoremap zO zCzO

function! MyFoldText() " {{{
  let line = getline(v:foldstart)

  let nucolwidth = &fdc + &number * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 3
  let foldedlinecount = v:foldend - v:foldstart

  " expand tabs into spaces
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')

  let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
  let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
  return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}
set foldtext=MyFoldText()
"----------Splits---------------
" bind Ctrl+<movement> keys to move around the windows, instead of using Ctrl+w + <movement>
" Every unnecessary keystroke that can be saved is good for your health :)
if exists('$TMUX')
  let g:tmux_navigator_no_mappings = 1
  let g:tmux_navigator_save_on_switch = 1
  nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
  nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
  nnoremap <silent> <c-j> :TmuxNavigateDown<cr>
  nnoremap <silent> <c-k> :TmuxNavigateUp<cr>
else
  nnoremap <c-j> <c-w>j
  nnoremap <c-k> <c-w>k
  nnoremap <c-l> <c-w>l
  nnoremap <c-h> <c-w>h
endif
"------------Meta/Config---------------
" Make it super easy to edit VimRC
:nnoremap <leader>ev :vsplit $MYVIMRC<cr>
:nnoremap <leader>sv :source $MYVIMRC<cr>

"-----------------Syntax Stuff------------------------------
set t_Co=256


if has ("autocmd")
  filetype on
endif

filetype plugin indent on
syntax on
colorscheme default

" Show whitespace
highlight SpecialKey ctermfg=DarkGray
set listchars=tab:>-,trail:·
set list

nnoremap <Leader>w :%s/\s\+$//e<Cr>:echo "Cleared Whitespace"<Cr><C-o>
" nnoremap <Leader>m :%s/\r//g<Cr><C-o>
nnoremap <Leader>m :!make<cr>


:augroup whitespaceGroup
:autocmd!
:autocmd FileType c,cpp,java,php autocmd BufWritePre <buffer> :%s/\s\+$//e
:augroup end

" Showing line numbers and length
set number" show line numbers
set tw=80 " width of document (used by gd)
set wrap
set wrapmargin=0

"set nowrap" don't automatically wrap on load
set fo-=t " don't automatically wrap text when typing
set colorcolumn=80

"------------History------------------
" Useful settings
set history=1000
set undodir=~/.vim/undodir
set undofile
set undolevels=800
set undoreload=10000
"------------COMPLETION---------------
"Use TAB to complete when typing words, else inserts TABs as usual.
"Uses dictionary and source files to find matching words to complete.

"See help completion for source,
"Note: usual completion is on <C-n> but more trouble to press all the time.
"Never type the same word twice and maybe learn a new spellings!
"Use the Linux dictionary when spelling is in doubt.
"Window users can copy the file to their machine.
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
:set dictionary="/usr/dict/words"
"---------------Backups------------------------
" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set backup                        " enable backups
set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files
set noswapfile
" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
  call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
  call mkdir(expand(&directory), "p")
endif
"--------------Misc---------------------

nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<esc>
" Insert new Line with Enter
nnoremap <S-Enter> O<Esc>
nnoremap <CR> o<Esc>

"--------Functions----------
function! NumberToggle()
  if(&relativenumber == 1)
    set relativenumber!
    set number
  else
    set relativenumber!
  endif
endfunc
function! ShowColourSchemeName()
  try
    echo g:colors_name
    return g:colors_name
  catch /^Vim:E121/
    return 0
  endtry
endfunction
nnoremap <leader>color :call ShowColourSchemeName()<cr>
nnoremap ;; $a;<esc>
"--------Autocommands-------
" Automatic reloading of .vimrc
:augroup writegroup
: autocmd!
: autocmd bufwritepost .vimrc source %
:augroup END

"Header macro
" Hm...I really need to get these asterisks down...
nnoremap <leader>ghead ggO*********************Author Info*************************<cr>@author    Christopher Findeisen                          <CR>@contact    <cfindeisen7@gmail.com>                              <CR>@date      <Esc>"=strftime("%c")<CR>Pa                                              <CR>*********************************************************<esc>gg$hl<C-v>4gjA*<esc>gglh<C-v>4gjI*<esc> gglhvjjjjzf<esc>:call MacroComment()<cr><cr>
function! MacroComment()
  normal gcc
endfunction

nnoremap <leader>start :.!cat ~/.vim/text/cpp.txt<cr>

noremap <Leader>c :call ToggleHeader()<cr>
function! ToggleHeader()
  if (expand('%:e') == 'cpp' || expand('%:e') == 'cc')
    :e %:r.h
  elseif(expand('%:e') == 'h')
    :e %:r.cpp
  else
    echo("Not in a cpp file")
  endif
endfunc
"set splits intuitively
set splitbelow
set splitright
