# custom_fzf.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

export FZF_DEFAULT_COMMAND='git rev-parse 2>/dev/null && git ls-files -co --exclude-standard || fd -t f -c never'
export FZF_DEFAULT_OPTS='--reverse --inline-info'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="bfs -nocolor -mindepth 1 -type d"

bindkey '^r' fzf-insert-history
bindkey '^t' fzf-insert-files
bindkey '\ec' fzf-change-directory

bindkey '^x^b' fzf-git-checkout-branch

fzf-ghq-look() {
  BUFFER="ghq look $(ghq list | fzf --reverse --inline-info)"
  zle accept-line
}
zle -N fzf-ghq-look
bindkey '^x^g' fzf-ghq-look
