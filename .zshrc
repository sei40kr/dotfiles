#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

[ -z "$TMUX" ] && [ -z "$VIMRUNTIME" ] && [ -z "$ATOM_HOME" ] && {
  export FZF_TMUX=true;
  export FZF_TMUX_HEIGHT='25%';
  tmux new-session;
  exit;
}

# zmodload zsh/zprof
zmodload zsh/zpty

autoload -Uz add-zsh-hook \
    cdr \
    chpwd_recent_dirs \
    zmv

# Load environment variables
source "${HOME}/.zshenv"

# Load secret environment variables
[ -e "${HOME}/.zshenv.secret" ] && . "${HOME}/.zshenv.secret"

setopt append_history \
    auto_cd \
    auto_list \
    auto_menu \
    auto_pushd \
    extended_history \
    glob_dots \
    hist_ignore_all_dups \
    hist_ignore_space \
    hist_reduce_blanks \
    interactive_comments \
    no_beep \
    print_eight_bit \
    prompt_subst \
    pushd_ignore_dups \
    share_history
unsetopt list_beep

bindkey -e
bindkey '^[[1;3C' forward-word
bindkey '^[[1;3D' backward-word
bindkey '^[[Z' reverse-menu-complete

export GOPATH="${HOME}/.go"

path=(
  '/usr/local/opt/coreutils/libexec/gnubin'
  '/usr/local/share/git-core/contrib/diff-highlight'
  "${HOME}/.cabal/bin"
  "${HOME}/.cargo/bin"
  "${GOPATH}/bin"
  "${path[@]}"
)

[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ] && source "${HOME}/.sdkman/bin/sdkman-init.sh"

# Configure alias-tips
ZSH_PLUGINS_ALIAS_TIPS_TEXT='alias-tips: '
ZSH_PLUGINS_ALIAS_TIPS_FORCE=true

# Configure emoji-cli
[ "$FZF_TMUX" != false ] && EMOJI_CLI_FILTER="fzf-tmux -d ${FZF_TMUX_HEIGHT}" || EMOJI_CLI_FILTER='fzf'

# Configure fzf
export FZF_DEFAULT_COMMAND='(git ls-files -co --exclude-standard || fd -Hn)'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd -Hnt d'

# Configure zsh-nvm
export NVM_LAZY_LOAD=true
export NVM_AUTO_USE=true
export NVM_SYMLINK_CURRENT=true

# Configure spaceship-zsh-theme
SPACESHIP_PROMPT_SYMBOL='❯'
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_BATTERY_SHOW=false

# Load zsh plugins
source "${ZSH_RC_DIR}/plugins.rc.zsh"

# Configure anyframe
zstyle ':anyframe:selector:fzf-tmux:' command "fzf-tmux -d ${FZF_TMUX_HEIGHT}"

bindkey '^r' anyframe-widget-execute-history
bindkey '^xb' anyframe-widget-cdr
bindkey '^xk' anyframe-widget-kill
bindkey '^x^k' anyframe-widget-kill
bindkey '^xe' anyframe-widget-insert-git-branch
bindkey '^x^e' anyframe-widget-insert-git-branch
bindkey '^x^b' anyframe-widget-checkout-git-branch
bindkey '^xg' anyframe-widget-cd-ghq-repository
bindkey '^x^g' anyframe-widget-cd-ghq-repository

source "${ZSH_RC_DIR}/aliases.rc.zsh"

