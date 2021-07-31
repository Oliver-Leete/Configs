
# set PATH /usr/local/go/bin /home/oleete/.cargo/bin $PATH

starship init fish | source

set -Ux EDITOR nvim
set -Ux VISUAL nvim
set -gx PATH /home/oleete/.cargo/bin /home/oleete/.yarn/bin /home/oleete/go/bin /home/oleete/.config/nvim/node_modules/tree-sitter-cli $PATH

export BAT_PAGER="less -R"
# aliases
alias exa "exa --colour=always --group-directories-first"
alias neovim nvim
alias n nvim
alias shutdown "sudo ~/.local/bin/shutdownupdate shutdown now"
alias reboot "sudo ~/.local/bin/shutdownupdate reboot now"
abbr vidir "vidir -v"
abbr ns "n -S"
abbr mv "mv -iv"
abbr cp "cp -riv"

abbr -a bk backup
abbr -a re restore
abbr -a mc "mkdir-cd -vp"
abbr -a unzip unzip-cd



# abbr  code "code-insiders"
# abbr  lcd "exa -1"
# abbr  ls "exa -a -l --git"
# abbr  lt "exa -a -l -T --level=2 --git"
# abbr  lg "exa -a -l -T --level=2 --git --git-ignore"
# abbr  cat "batcat"
# abbr  fd "fd -H -L -I -c=always"
# abbr  fdg "fd -g"
# abbr  sed "sed -E"
# abbr
# abbr  .. "cd .."
# abbr  grep rg
# abbr  cp "cp -i"
# abbr  mv "mv -i"
# abbr  rm "rm -i"
# abbr  df "df -h"
# abbr  free "free -m"
# abbr  psmem 'ps auxf | sort -nr -k 4 | head -10'
# abbr  pscpu 'ps auxf | sort -nr -k 3 | head -10'
# abbr  htop 'htop -t'
# abbr
alias conf '/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'
alias lazyconf 'lazygit --git-dir=$HOME/dotfiles --work-tree=$HOME'
# abbr
# abbr  fzf "fzf --preview 'batcat --color=always --style=numbers --line-range=:500 {}'"
# abbr  man "batman"
# abbr  rb "batgrep"
# abbr  watch "batwatch"

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
abbr lzg lazygit


function bind_bang
    switch (commandline -t)
        case "!"
            commandline -t $history[1]
            commandline -f repaint
        case "*"
            commandline -i !
    end
end

function bind_dollar
    switch (commandline -t)
        case "!"
            commandline -t ""
            commandline -f history-token-search-backward
        case "*"
            commandline -i '$'
    end
end

function fish_user_key_bindings
    bind ! bind_bang
    bind '$' bind_dollar
end

function take
    mkdir -p $argv
    cd $argv
end

function backup --argument filename
    cp $filename $filename.bak
end
function restore --argument file
    mv $file (echo $file | sed s/.bak//)
end
function mkdir-cd
    mkdir $argv && cd
end

function clean-unzip --argument zipfile
    if not test (echo $zipfile | string sub --start=-4) = .zip
        echo (status function): argument must be a zipfile
        return 1
    end

    if is-clean-zip $zipfile
        unzip $zipfile
    else
        set zipname (echo $zipfile | trim-right '.zip')
        mkdir $zipname || return 1
        unzip $zipfile -d $zipname
    end
end
function unzip-cd --argument zipfile
    clean-unzip $zipfile && cd (echo $zipfile | trim-right .zip)
end

function clean-unzip --argument zipfile
    if not test (echo $zipfile | string sub --start=-4) = .zip
        echo (status function): argument must be a zipfile
        return 1
    end

    if is-clean-zip $zipfile
        unzip $zipfile
    else
        set zipname (echo $zipfile | trim-right '.zip')
        mkdir $zipname || return 1
        unzip $zipfile -d $zipname
    end
end
function skip-lines --argument n
    tail +(math 1 + $n)
end
function take --argument number
    head -$number
end
function unsymlink --argument _file
    set file (echo $_file | trim-trailing-slash)
    test -L $file
    and rm $file
end
function isodate
    date +%Y-%m-%d
end

function wip
    if git diff --cached --quiet
        git add .
    end
    git commit --no-verify -m "wip $argv"
end

clear
# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/oleete/.ghcup/env; and set -gx PATH $HOME/.cabal/bin /home/oleete/.ghcup/bin $PATH

# TokyoNight Color Palette
set -l foreground c0caf5
set -l selection 33467C
set -l comment 565f89
set -l red f7768e
set -l orange ff9e64
set -l yellow e0af68
set -l green 9ece6a
set -l purple 9d7cd8
set -l cyan 7dcfff
set -l pink bb9af7

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
