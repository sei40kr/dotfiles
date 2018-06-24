#!/usr/bin/env fish

# fzf.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -Ux FZF_DEFAULT_COMMAND 'git ls-files -co --exclude-standard 2>/dev/null || fd -t f -c never'
set -Ux FZF_DEFAULT_OPTS '--reverse --inline-info'
set -Ux FZF_FIND_FILE_COMMAND 'begin; git ls-files -co --exclude-standard ^/dev/null; or fd -t f -c never; end'
set -Ux FZF_CD_WITH_HIDDEN_COMMAND 'bfs -nocolor -mindepth 1 -type d'
