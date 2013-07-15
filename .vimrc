"非兼容模式
set nocp
"底部标尺
set ru
"搜索时高亮显示结果
set hls
"输入搜索命令时，显示目前输入的模式的匹配位置
set is
"显示行号
set number
"使用:e命令的时候 可选项预览
set wildmenu
"文件默认编码
set enc=utf-8
"文件保存时使用的编码
"fileencoding=utf-8
"打开文件进行解码的猜测列表，优先以utf-8打开
set fileencodings=utf-8,gbk
"文件默认格式unix
set ff=unix
"缩进为4个空格宽度
set tabstop=4
"自动缩进使用的空格数
set shiftwidth=4
"编辑插入tab时，把tab算作的空格数
set softtabstop=4
"缩进使用空格
set expandtab
"自动缩进
set autoindent
"鼠标捕捉
set mouse=a
"打开语法高亮
syntax on

set comments=sl:/*,mb:*,ex:*/
"设置字典
autocmd FileType javascript set dictionary=~/.vim/dict/javascript.dict
"默认配色为darkblue
color darkbluefix
"显示标签栏 0: 从不 1: 大于1个时显示 2: 总是
set showtabline=2

"设置制表符、回车、空格的显示
"set list
set listchars=tab:▸\ ,trail:·,eol:¬,nbsp:_

"设置backspace键可以删除行首
set backspace=indent,eol,start

"使Command-T插件忽略yui目录
set wildignore=*static/yui*
"切换buffer不用强制保存
set hidden

" 重新载入配置
map <leader>s :source ~/.vimrc<CR>

" visual模式修改缩进时不退出该模式
vnoremap < <gv
vnoremap > >gv 

"映射F12键为行号的开关
map <F12> :set number!<CR>

nmap <C-N> :tabnext<CR>
nmap <C-P> :tabprevious<CR>

"nnoremap <C-c> :update<CR>
"cnoremap <C-c> <Esc>:update<CR>
"inoremap <C-c> <Esc>:update<CR>

" insert mode shortcut
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>
inoremap <C-d> <DELETE>

" command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>

" 打开php折叠
let php_folding = 1 
" 打开javascript折叠
let b:javascript_fold=1
" 打开javascript对dom、html和css的支持
let javascript_enable_domhtmlcss=1

"打开文件类型检测
filetype on
"为特定文件类型允许插件文件的载入
filetype plugin on
filetype plugin indent on
au BufNewFile,BufRead *.as set filetype=actionscript
" Multiple filetype for freemarker
au BufNewFile,BufRead *.ftl set filetype=ftl.html
au BufReadCmd *.jar,*.xpi,*.egg call zip#Browse(expand("<amatch>"))
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType css set foldmethod=marker
autocmd FileType python filetype plugin indent on
autocmd FileType python setlocal et sta sw=4 sts=4

" 设置javascriptlint
"autocmd FileType javascript set makeprg=jslint\ %
"autocmd FileType javascript set errorformat=%f(%l):\ %m
" 设置jshint
autocmd FileType javascript set makeprg=jshint\ --config\ ~/projects/fe.tools/jshint/jshintrc\ %
autocmd FileType javascript set errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m

autocmd FileType javascript inoremap <silent> <F9> <C-O>:make<CR>
autocmd FileType javascript map <silent> <F9> :make<CR>

" 打开文件时总是跳到最后光标所在的行
autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif

" In visual mode, git blame the selection
function! GitBlame() range
" look up function-range-example for more information
    let beg_line = line("'<")
    let end_line = line("'>")
    exec '!git blame -L '. beg_line. ','. end_line. ' %'
endfunction
vnoremap <leader>g :call GitBlame()<CR>
" In normal mode, git blame the current line
nnoremap <leader>g :exec '!git blame -L '. line("."). ','. line("."). ' %'<CR>

"初始化所有插件
function! BundlesInit()
    let bundles = {
            \'vim-pathogen' : 'github.com/tpope/vim-pathogen.git',
            \'vim-fugitive' : 'github.com/tpope/vim-fugitive.git',
            \'nerdtree' : 'github.com/scrooloose/nerdtree.git',
            \'nerdcommenter' : 'github.com/scrooloose/nerdcommenter.git',
            \'ctrlp.vim' : 'github.com/kien/ctrlp.vim.git',
            \'command-t' : 'git.wincent.com/command-t.git',
            \'snipmate.vim' : 'github.com/msanders/snipmate.vim.git',
            \'tagbar' : 'github.com/majutsushi/tagbar.git',
            \'vim-taglist-plus' : 'github.com/int3/vim-taglist-plus.git',
            \'zencoding-vim' : 'github.com/mattn/zencoding-vim.git',
            \'syntastic' : 'github.com/scrooloose/syntastic.git',
            \'vim-easymotion' : 'github.com/Lokaltog/vim-easymotion.git',
            \'vim-node.js' : 'github.com/mmalecki/vim-node.js.git',
            \'vim-colors-solarized' : 'github.com/altercation/vim-colors-solarized.git',
            \'vim-vividchalk' : 'github.com/tpope/vim-vividchalk.git'
        \}
    let bundleDir = $HOME . '/.vim/bundle/'
    if !isdirectory(bundleDir)
        let output = mkdir(bundleDir)
    endif

    for key in keys(bundles)
        let dir = bundleDir . key
        if !isdirectory(dir)
            let cmd = 'git clone git://' . bundles[key] . ' ' . bundleDir . key
            "execute cmd
            echo 'fetching ' . key . '...'
            let output = system(cmd)
        endif
    endfor

    if exists(':Helptags')
        :Helptags
    endif

    echo 'all bundles are ready.'
endfunction
nnoremap <leader>h :call BundlesInit()<CR>

"初始化配置及pathogen
function! VimInitAll()
    "载入本地扩展配置文件
    let vimrc_extend  = $HOME . "/.vimrc.ext"
    if filereadable(vimrc_extend)
        execute "source " . vimrc_extend
    endif

    "初始化pathogen插件
    let pathogen = $HOME . '/.vim/bundle/vim-pathogen/autoload/pathogen.vim'
    if !filereadable(pathogen)
        call BundlesInit()
    endif

    execute "source " . pathogen
    call pathogen#infect()

endfunction

"CtrlP插件设置
let g:ctrlp_map = '<leader>p'
let g:ctrlp_by_filename = 1

"Command-T插件设置
let g:CommandTMaxHeight = 10
let g:CommandTMinHeight = 10
let g:CommandTCancelMap = ['<Esc>', '<C-c>']

"NERDTree插件设置
let NERDTreeWinPos = 'left'
nnoremap <leader>nt :NERDTree<CR>

"syntastic插件设置
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': ['dddjavascript', 'ruby'] }
let g:syntastic_auto_loc_list = 1
"let g:syntastic_javascript_gjslint_conf = '--strict'
nnoremap <leader>sc :SyntasticCheck<CR>

call VimInitAll()

""""""""""""""""""""""""" below is for testing """""""""""""""""""""""""

" solarized
set background=dark
set cursorline
let g:solarized_termtrans = 1
colorscheme solarized
" fix folded line color
hi Folded   guifg=#808080 guibg=#000040 ctermfg=lightblue ctermbg=black cterm=bold term=bold
" fix tabline
"hi TabLine     cterm=none ctermfg=lightgrey ctermbg=lightblue guifg=gray guibg=black
"hi TabLineSel  cterm=none ctermfg=lightgrey ctermbg=LightMagenta guifg=white guibg=black
"hi TabLineFill cterm=none ctermfg=lightblue ctermbg=lightblue guifg=black guibg=black 
" solarized end

let g:EasyMotion_leader_key = ',,'


"""""""""""""""""""""""""""""
" TagList setting
"""""""""""""""""""""""""""""
"Exuberant ctags程序的位置
"let Tlist_Ctags_Cmd="/opt/local/bin/ctags"
"let Tlist_Ctags_Cmd="/usr/local/bin/jsctags"
"let Tlist_Inc_Winwidth=100
"let Tlist_WinWidth='auto'
"在右侧窗口打开
"let Tlist_Use_Right_Window=1
"只显示当前文件的tag
"let Tlist_File_Fold_Auto_Close=1
"如果Taglist是最后一个窗口则退出vim
"let Tlist_Exit_OnlyWindow = 1
"let g:tlist_javascript_settings = 'javascript;s:string;a:array;o:object;f:function'
let g:tlist_javascript_settings = 'javascript;f:function;c:class;o:object;m:method;s:string;a:array;n:constant'

"let g:tagbar_ctags_bin = '/usr/local/bin/jsctags'

""""""""""""""""""""""""""""""
" BufExplore settingr
""""""""""""""""""""""""""""""
let g:bufExplorerDefaultHelp=0       " Do not show default help.
let g:bufExplorerShowRelativePath=1  " Show relative paths.
let g:bufExplorerSortBy='mru'        " Sort by most recently used.
let g:bufExplorerSplitRight=0        " Split left.
let g:bufExplorerSplitVertical=1     " Split vertically.
let g:bufExplorerSplitVertSize = 30  " Split width
let g:bufExplorerUseCurrentWindow=1  " Open in new window.

""""""""""""""""""""""""""""""
" winManager setting
""""""""""""""""""""""""""""""
"let g:winManagerWindowLayout = \""BufExplorer,FileExplorer|TagList"
let g:winManagerWindowLayout = "FileExplorer"
let g:winManagerWidth = 30
let g:defaultExplorer = 0
nmap <C-W><C-F> :FirstExplorerWindow<CR>
nmap <C-W><C-B> :BottomExplorerWindow<CR>
nmap <silent> <leader>wm :WMToggle<CR> 



