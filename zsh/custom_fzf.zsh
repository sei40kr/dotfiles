# custom_fzf.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

export FZF_DEFAULT_COMMAND='git rev-parse 2>/dev/null && git ls-files -co --exclude-standard || fd -t f -c never'
export FZF_DEFAULT_OPTS="
  --reverse
  --inline-info
  --color fg:'#b2b2b2',bg:'#262626',hl:'#268bd2',fg+:'#b2b2b2',bg+:'#444444',hl+:'#268bd2'
  --color info:'#b1951d',prompt:'#b1951d',pointer:'#b2b2b2',marker:'#b2b2b2'
  --no-bold"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="bfs -nocolor -mindepth 1 -type d"

export DOT_BASE_DIR="$(realpath "$(dirname "${(%):-%N}")/..")"

bindkey '^r' fzf-insert-history
bindkey '\ec' fzf-change-directory

bindkey '^x^b' fzf-git-checkout-branch
bindkey '^x.' fzf-edit-dotfiles

my-fzf-insert-files() {
  __fzf::widget::init || return 1

  eval "$FZF_DEFAULT_COMMAND" |
    __fzf::widget::select -m | \
    __fzf::widget::insert -q
}
zle -N my-fzf-insert-files
bindkey '^t' my-fzf-insert-files

my-fzf-ghq-look() {
  __fzf::widget::init 'ghq look' || return 1

  ghq list | \
    __fzf::widget::select | \
    __fzf::widget::insert

  __fzf::widget::exec
}
zle -N my-fzf-ghq-look
bindkey '^x^g' my-fzf-ghq-look

my-fzf-toggl-start-todoist() {
  __fzf::widget::init 'toggl start' || return 1

  todoist --csv list | \
    awk -F',' '{ print $6 }' | \
    __fzf::widget::select | \
    __fzf::widget::insert -q

  __fzf::widget::exec
}
zle -N my-fzf-toggl-start-todoist
bindkey '^x^t' my-fzf-toggl-start-todoist
