"   _____ ______   ________  ________   ________  ________  ___      ___ ___  ________
"  |\   _ \  _   \|\   __  \|\   ___  \|\   __  \|\   __  \|\  \    /  /|\  \|\   __  \
"  \ \  \\\__\ \  \ \  \|\  \ \  \\ \  \ \  \|\  \ \  \|\  \ \  \  /  / | \  \ \  \|\  \
"   \ \  \\|__| \  \ \  \\\  \ \  \\ \  \ \   _  _\ \  \\\  \ \  \/  / / \ \  \ \   __  \
"    \ \  \    \ \  \ \  \\\  \ \  \\ \  \ \  \\  \\ \  \\\  \ \    / /   \ \  \ \  \ \  \
"     \ \__\    \ \__\ \_______\ \__\\ \__\ \__\\ _\\ \_______\ \__/ /     \ \__\ \__\ \__\
"      \|__|     \|__|\|_______|\|__| \|__|\|__|\|__|\|_______|\|__|/       \|__|\|__|\|__|


" A Vim colorscheme
" Author:       Alessandro Yorba
" Script URL:   https://github.com/AlessandroYorba/Monrovia
" License:      MIT


"=================================================================================
" BASIC TERMINAL COLORS:
"=================================================================================
"     Foreground    #BADDF1             |      Selection     #BADDF1             |
"     Background    #1F2433             |      Selected Text #1F2433             |
"     CursorColor   #FFFFFF             |                                        |
"     Bold          #FFFFFF             |                                        |
"     Links         #CB2656             |                                        |
"=================================================================================
" ANSI COLORS:
"=================================================================================
"---------------------------------------|-----------------------------------------
"     NORMAL        Hex          xterm  |      BRIGHT        Hex          xterm  |
"---------------------------------------|-----------------------------------------
"     Black         #1F2433      0      |      brBlack       #375268      8      |
"     Red           #8c3432      1      |      brRed         #8c3432      9      |
"     Green         #247dae      2      |      brGreen       #247dae      10     |
"     Yellow        #7095b4      3      |      brYellow      #7095b4      11     |
"     Blue          #7f4c60      4      |      brBlue        #7f4c60      12     |
"     Magenta       #cb2656      5      |      brMagenta     #cb2656      13     |
"     Cyan          #8cb4c0      6      |      brCyan        #8cb4c0      14     |
"     White         #BADDF1      7      |      brWhite       #FFFFFF      15     |
"---------------------------------------|-----------------------------------------

"=================================================================================
" SETUP:
"=================================================================================

set background=dark

highlight clear
if exists("syntax_on")
    syntax reset
endif

"=======================================================================================================================
" LOAD DEFAULT PARAMETERS:
"=======================================================================================================================

let g:colors_name="monrovia"

"=======================================================================================================================
" COMMON MONROVIA HIGHLIGHTING:
"=======================================================================================================================
highlight! Normal                   guifg=#BADDF1  guibg=#1F2433  gui=NONE       ctermfg=7     ctermbg=0     cterm=NONE
highlight! LineNr                   guifg=#2F4659  guibg=NONE     gui=NONE       ctermfg=8     ctermbg=NONE  cterm=NONE
highlight! TabLine                  guifg=NONE     guibg=NONE     gui=NONE       ctermfg=NONE  ctermbg=NONE  cterm=NONE 
highlight! TabLineSel               guifg=#1F2433  guibg=#7095b4  gui=NONE       ctermfg=0     ctermbg=3     cterm=NONE
highlight! TabLineFill              guifg=#375268  guibg=NONE     gui=underline  ctermfg=8     ctermbg=NONE  cterm=underline
highlight! CursorLine               guifg=NONE     guibg=#232839  gui=NONE       ctermfg=NONE  ctermbg=0     cterm=NONE
highlight! CursorColumn             guifg=NONE     guibg=#1F2433  gui=NONE       ctermfg=NONE  ctermbg=0     cterm=NONE
highlight! StatusLine               guifg=#BADDF1  guibg=#375268  gui=NONE       ctermfg=7     ctermbg=8     cterm=NONE
highlight! StatusLineNC             guifg=#1F2433  guibg=#375268  gui=NONE       ctermfg=0     ctermbg=8     cterm=NONE
highlight! VertSplit                guifg=#375268  guibg=NONE     gui=NONE       ctermfg=8     ctermbg=0     cterm=NONE
highlight! Visual                   guifg=#7095b4  guibg=NONE     gui=reverse    ctermfg=3     ctermbg=NONE  cterm=reverse
highlight! PmenuSbar                guifg=#375268  guibg=#375268  gui=NONE       ctermfg=8     ctermbg=8     cterm=NONE
highlight! Comment                  guifg=#375268  guibg=NONE     gui=NONE       ctermfg=8     ctermbg=NONE  cterm=NONE
highlight! SpecialComment           guifg=#375268  guibg=NONE     gui=reverse    ctermfg=8     ctermbg=NONE  cterm=reverse
highlight! Function                 guifg=#7095b4  guibg=NONE     gui=NONE       ctermfg=3     ctermbg=NONE  cterm=NONE
highlight! Error                    guifg=#8C3432  guibg=NONE     gui=reverse    ctermfg=1     ctermbg=NONE  cterm=reverse
highlight! SpellBad                 guifg=#8C3432  guibg=NONE     gui=undercurl  ctermfg=1     ctermbg=NONE  cterm=undercurl
highlight! SpellLocal               guifg=#A1617A  guibg=NONE     gui=undercurl  ctermfg=2     ctermbg=NONE  cterm=undercurl
highlight! SpellCap                 guifg=#247DAE  guibg=NONE     gui=undercurl  ctermfg=3     ctermbg=NONE  cterm=undercurl

"=======================================================================================================================
" MONROVIA:
"=======================================================================================================================
if 1
    highlight! PreProc              guifg=#7f4c60  guibg=NONE     gui=NONE       ctermfg=4     ctermbg=NONE  cterm=NONE
    highlight! String               guifg=#247DAE  guibg=NONE     gui=NONE       ctermfg=2     ctermbg=NONE  cterm=NONE
    highlight! Search               guifg=#7f4c60  guibg=NONE     gui=reverse    ctermfg=4     ctermbg=NONE  cterm=reverse
    highlight! DiffAdd              guifg=#BADDF1  guibg=NONE     gui=reverse    ctermfg=7     ctermbg=NONE  cterm=reverse
    highlight! DiffChange           guifg=#247DAE  guibg=NONE     gui=reverse    ctermfg=5     ctermbg=NONE  cterm=reverse
    highlight! MatchParen           guifg=#1F2433  guibg=#7f4c60  gui=NONE       ctermfg=0     ctermbg=4     cterm=NONE
    highlight! SpecialKey           guifg=#538798  guibg=NONE     gui=NONE       ctermfg=6     ctermbg=0     cterm=NONE
    highlight! Todo                 guifg=#247DAE  guibg=NONE     gui=reverse    ctermfg=2     ctermbg=NONE  cterm=reverse
endif

highlight! link DiffDelete          Error
highlight! link ErrorMsg            Error
highlight! link WildMenu            Visual
highlight! link ModeMsg             Visual
highlight! link StorageClass        Normal
highlight! link htmlItalic          Normal
highlight! link SignColumn          String
highlight! link MoreMsg             String
highlight! link markdownLinkText    String
highlight! link WarningMsg          String
highlight! link Title               PreProc
highlight! link Number              PreProc
highlight! link FoldColumn          PreProc
highlight! link phpVarSelector      PreProc
highlight! link Directory           PreProc
highlight! link cssIdentifier       PreProc
highlight! link cssClassName        PreProc
highlight! link Identifier          Function
highlight! link Special             Function
highlight! link Type                Function
highlight! link Statement           Function
highlight! link Constant            Function
highlight! link Operator            Function
highlight! link markdownUrl         SpecialKey
highlight! link Underlined          SpecialKey
highlight! link Question            SpecialKey
highlight! link Pmenu               StatusLineNC
highlight! link TabLine             StatusLineNC
highlight! link VimCommentTitle     SpecialComment
highlight! link Folded              LineNr
highlight! link NonText             Comment
highlight! link DiffText            DiffAdd
highlight! link PmenuThumb          PmenuSbar
highlight! link ColorColumn         CursorLine
highlight! link CursorLineNr        TabLineSel
highlight! link PmenuSel            StatusLine
highlight! link SpellRare           SpellLocal
