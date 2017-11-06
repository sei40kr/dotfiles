#!/usr/bin/env zsh

# 80_custom.zsh
# author: Seong Yong-ju ( @sei40kr )

# mollifier/anyframe {{{
anyframe-widget-cd-ghq-repository() {
  anyframe-source-ghq-repository | \
      anyframe-selector-auto | \
      anyframe-action-execute builtin cd --
}
# }}}
