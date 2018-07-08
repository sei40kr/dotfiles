#!/usr/bin/env fish

# fzf.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -Ux FZF_DEFAULT_COMMAND 'git ls-files -co --exclude-standard 2>/dev/null || fd -t f -c never'
set -Ux FZF_DEFAULT_OPTS '--reverse --inline-info'

if type -q fd
    set -Ux FZF_FIND_FILE_COMMAND 'begin; git ls-files -co --exclude-standard ^/dev/null; or fd -t f -c never; end'
else
    set -Ux FZF_FIND_FILE_COMMAND 'begin; git ls-files -co --exclude-standard ^/dev/null; or find -type f; end'
end

if type -q bfs
    set -Ux FZF_CD_WITH_HIDDEN_COMMAND 'bfs -nocolor -mindepth 1 -type d'
else
    set -Ux FZF_CD_WITH_HIDDEN_COMMAND 'find -mindepth 1 -type d'
end
