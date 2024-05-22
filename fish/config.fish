# set PATH /usr/local/go/bin /home/oleete/.cargo/bin $PATH

starship init fish | source

set -Ux BROWSER "/home/oleete/.config/bin/browser"
set -gx BROWSER "/home/oleete/.config/bin/browser"
set -Ux EDITOR "/home/oleete/.config/bin/nvrStart"
set -gx EDITOR "/home/oleete/.config/bin/nvrStart"
set -Ux VISUAL "/home/oleete/.config/bin/nvrStart"
set -gx VISUAL "/home/oleete/.config/bin/nvrStart"
set -Ux PYENV_ROOT "/home/oleete/.pyenv"
fish_add_path --path $PYENV_ROOT/bin
fish_add_path --path /home/oleete/.cargo/bin
fish_add_path --path /home/oleete/.yarn/bin
fish_add_path --path /home/oleete/go/bin
fish_add_path --path /home/oleete/.config/nvim/node_modules/tree-sitter-cli
fish_add_path --path /home/oleete/.local/bin
fish_add_path --path /home/oleete/.config/bin
fish_add_path --path /home/oleete/.cabal/bin
fish_add_path --path /home/oleete/.ghcup/bin

pyenv init - | source

# aliases
alias neovim nvim
alias n nvrStart
alias ne "n +LoadSession"

abbr cd z
abbr vidir "echo 'use edir'"
abbr rm "echo use trm"
abbr trm "trash-put -i"
abbr redir "fd -HLI | edir"
abbr dust "dua i"

abbr mv "mv -iv"
abbr cp "cp -riv"

abbr fd "fd -HLI"

abbr ls "eza -1 --hyperlink --icons --group-directories-first"
abbr lr "eza -1 --hyperlink --group-directories-first"
abbr ll "eza -aal --hyperlink --icons --group-directories-first --time-style relative"
abbr lt "eza -alT --hyperlink --level=2 --icons --group-directories-first --time-style relative"
abbr lg "eza -alT --hyperlink --git-ignore --icons --group-directories-first --time-style relative"

abbr -a bk backup
abbr -a re restore

alias edrive "cd /run/media/oleete"

abbr htop 'htop -t'
alias lzg lazygit
alias lzd lazydocker
alias zathura "zathura --fork"

# Git abbreviations
alias g git
abbr gc "g checkout"
abbr gcm "g commit"
abbr ga "g add"
abbr gp "g pull"
abbr gP "g push"
abbr gb "g branch"
abbr gs "g status -sb"
abbr gr "g remote -v"
abbr gd "g diff"
abbr gdt "g difftool"
abbr glast "g log -1 HEAD --stat"
abbr gloga "g log --all"
abbr glogg "g log --oneline --graph --decorate --all "
abbr gmt "g mergetool"
abbr gmt2 "g mergetool --tool nvimdiff"
abbr gclone clone
abbr gwip wip

function mkmv
    mkdir -p $argv
    cd $argv
end

function backup --argument filename
    cp $filename $filename.bak
end

function restore --argument file
    mv $file (echo $file | sed s/.bak//)
end

function isodate
    date +%Y-%m-%d
end

function manpdf --argument program
    man -Tpdf $program | zathura -
end

function help
    $argv --help 2>&1 | bat --language=help -pp
end

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/oleete/.ghcup/env; and set -gx PATH $HOME/.cabal/bin /home/oleete/.ghcup/bin $PATH

set -l foreground DCD7BA
set -l selection 2D4F67
set -l comment 727169
set -l red C34043
set -l orange FF9E64
set -l yellow C0A36E
set -l green 76946A
set -l purple 957FB8
set -l cyan 7AA89F
set -l pink D27E99

# Syntax Highlighting Colors
set -g fish_color_normal $foreground
set -g fish_color_command $cyan
set -g fish_color_keyword $pink
set -g fish_color_quote $yellow
set -g fish_color_redirection $foreground
set -g fish_color_end $orange
set -g fish_color_error $red
set -g fish_color_param $purple
set -g fish_color_comment $comment
set -g fish_color_selection --background=$selection
set -g fish_color_search_match --background=$selection
set -g fish_color_operator $green
set -g fish_color_escape $pink
set -g fish_color_autosuggestion $comment

# Completion Pager Colors
set -g fish_pager_color_progress $comment
set -g fish_pager_color_prefix $cyan
set -g fish_pager_color_completion $foreground
set -g fish_pager_color_description $comment
set fish_greeting

zoxide init fish | source
# clear
