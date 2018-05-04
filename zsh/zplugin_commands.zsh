# zplugin_commands.zsh --- ZSH plugins to add commands
# author: Seong Yong-ju <sei40kr@gmail.com>

# cd-gitroot

cd-gitroot() {
  unhash -f cd-gitroot
  zplugin light mollifier/cd-gitroot

  cd-gitroot "$@"
}
alias U='cd-gitroot'

# direnv

zplugin ice from'gh-r' as'program' mv'direnv* -> direnv' atclone'./direnv hook zsh >zhook.zsh' atpull'%atclone' src'zhook.zsh'
zplugin light direnv/direnv

# extract

extract() {
  unhash -f extract
  unalias x

  zplugin ice svn
  zplugin snippet OMZ::plugins/extract

  extract "$@"
}
alias x='extract'

# fzf-widgets

zplugin light ytet5uy4/fzf-widgets

# gotcha

zplugin ice from'gh-r' as'program' mv'gotcha_* -> gotcha'
zplugin light b4b4r07/gotcha

# httpstat

zplugin ice as'program' pick'httpstat' mv'httpstat.sh -> httpstat'
zplugin light b4b4r07/httpstat

# ssh-keyreg

zplugin ice as'program' pick'bin/ssh-keyreg'
zplugin light b4b4r07/ssh-keyreg

# k

k() {
  unhash -f k
  zplugin ice pick'k.sh'
  zplugin light supercrabtree/k

  alias k='k -Ah --no-vcs'

  k "$@"
}
