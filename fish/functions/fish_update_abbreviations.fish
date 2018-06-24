#!/usr/bin/env fish

# fish_update_abbreviations.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function fish_update_abbreviations
    set -Ue _fish_abbreviations_initialized

    source (dirname (status --current-filename))/../conf.d/abbreviations.fish
end
