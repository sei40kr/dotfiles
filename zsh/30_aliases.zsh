alias u='builtin cd ..'

# Emacs
# cf https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/emacs/emacs.plugin.zsh

EMACS_PLUGIN_LAUNCHER="${ZDOTDIR}/plugins/emacs/emacsclient.sh"

alias emacs='LC_CTYPE=ja-JP.UTF-8 ${EMACS_PLUGIN_LAUNCHER} -n'
alias e=emacs
# open terminal ${EMACS_PLUGIN_LAUNCHER}
alias te='${EMACS_PLUGIN_LAUNCHER} -nw'
# same than M-x eval but from outside Emacs.
alias eeval='${EMACS_PLUGIN_LAUNCHER} -e'
# create a new X frame
alias eframe='emacsclient -a "" -c'

alias ekill='emacsclient -e "(kill-emacs)"'
