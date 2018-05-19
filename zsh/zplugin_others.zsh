# zplugin_others.zsh --- ZSH plugins
# author: Seong Yong-ju <sei40kr@gmail.com>

# alias-tips

export ZSH_PLUGINS_ALIAS_TIPS_TEXT="alias-tips: "
zplugin ice lucid wait'1'
zplugin light djui/alias-tips

# fast-syntax-highlighting

zplugin ice lucid atload'zpcompinit' wait'0'
zplugin light zdharma/fast-syntax-highlighting

# fzf

zplugin ice from'gh-r' as'program'; zplugin light junegunn/fzf-bin

# pure

VIM_PROMPT='»'
PURE_GIT_DOWN_ARROW='↑'
PURE_GIT_UP_ARROW='↓'
zplugin ice pick'async.zsh' lucid src'pure.zsh' wait'!0' atload'
  PROMPT="%(?.%F{magenta}.%F{red})\${editor_info[keymap]}%f "'

zplugin light sindresorhus/pure

# zsh-autosuggestions

zplugin ice lucid wait'0' atload'
  bindkey -M viins "^i" autosuggest-accept
  _zsh_autosuggest_start'
zplugin light zsh-users/zsh-autosuggestions

# zsh-better-run-help

zplugin light "${HOME}/.zsh/plugins/zsh-better-run-help"

# zsh-nvm

if [[ -n "$NVM_DIR" ]]; then
  export NVM_SYMLINK_CURRENT=true
  export NVM_NO_USE=true
  zplugin ice atload'nvm use --silent --delete-prefix v8.10.0'
  zplugin light lukechilds/zsh-nvm
fi

# zsh-tmux-rename

zplugin ice lucid wait'1' load'[[ -n "$TMUX" ]]' unload'[[ -z "$TMUX" ]]'
zplugin load sei40kr/zsh-tmux-rename
