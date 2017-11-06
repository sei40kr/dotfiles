#!/usr/bin/env zsh

# 20_key-bindings.zsh
# author: Seong Yong-ju ( @sei40kr )

bindkey '^[[1;3C' forward-word
bindkey '^[[1;3D' backward-word

# mollifier/anyframe {{{
bindkey '^xb' anyframe-widget-cdr
bindkey '^x^b' anyframe-widget-checkout-git-branch
bindkey '^xr' anyframe-widget-execute-history
bindkey '^x^r' anyframe-widget-execute-history
bindkey '^xi' anyframe-widget-put-history
bindkey '^x^i' anyframe-widget-put-history
bindkey '^xg' anyframe-widget-cd-ghq-repository
bindkey '^x^g' anyframe-widget-cd-ghq-repository
bindkey '^xk' anyframe-widget-kill
bindkey '^x^k' anyframe-widget-kill
bindkey '^xe' anyframe-widget-insert-git-branch
bindkey '^x^e' anyframe-widget-insert-git-branch
# }}}
