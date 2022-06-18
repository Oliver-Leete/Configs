# set PATH /usr/local/go/bin /home/oleete/.cargo/bin $PATH

starship init fish | source

set -Ux BROWSER "/home/oleete/.config/bin/browser"
set -gx BROWSER "/home/oleete/.config/bin/browser"
set -Ux EDITOR "/home/oleete/.config/bin/nvrTab --remote-tab-wait"
set -Ux VISUAL "/home/oleete/.config/bin/nvrTab --remote-tab-wait"
fish_add_path --path /home/oleete/.cargo/bin
fish_add_path --path /home/oleete/.yarn/bin
fish_add_path --path /home/oleete/go/bin
fish_add_path --path /home/oleete/.config/nvim/node_modules/tree-sitter-cli
fish_add_path --path /home/oleete/.local/bin
fish_add_path --path /home/oleete/.config/bin
fish_add_path --path /home/oleete/.cabal/bin
fish_add_path --path /home/oleete/.ghcup/bin

set WORKSPACE (wmctrl -d | grep '*' | awk "{print $NF}")
export PAGER="/home/oleete/.config/bin/nvrTab --remote-tab-wait"

# aliases
alias neovim nvim
alias n nvrStart
alias k kak
alias h hx

alias exa "exa --colour=always --group-directories-first --icons"

abbr vidir "vidir -v"
abbr mv "mv -iv"
abbr cp "cp -riv"
abbr rm "rm -i"

abbr  fd "fd -HLI"

abbr  ls "exa -1 --icons"
abbr  ll "exa -a -l --icons"
abbr  lt "exa -a -l -T --level=2 --icons"
abbr  lg "exa -a -l -T --level=2 --git --git-ignore --icons"

abbr -a bk backup
abbr -a re restore

alias .. "cd .."
alias ... "cd ../.."
alias .... "cd ../../.."
alias ..... "cd ../../../.."

abbr  htop 'htop -t'
alias lzg lazygit
alias lzd lazydocker
alias zathura zathura --fork

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
clear
