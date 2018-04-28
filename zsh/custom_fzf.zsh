# custom_fzf.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

export FZF_DEFAULT_COMMAND='git rev-parse 2>/dev/null && git ls-files -co --exclude-standard || fd -t f -c never'
export FZF_DEFAULT_OPTS='--reverse --inline-info'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="bfs -nocolor -mindepth 1 -type d"

fzf-kill() {
  ps -u "$USER" -o pid,command,%cpu,%mem \
    | fzf --reverse --inline-info --header-lines=1 \
    | awk '{ print $1 }' \
    | xargs kill
}
zle -N fzf-kill
bindkey '^x^k' fzf-kill
