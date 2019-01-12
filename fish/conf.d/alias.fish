# alias.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function save_alias_funcs
    set -l dir (realpath (dirname (status filename))'/../alias_defs')

    source $dir'/alias_def.fish'

    for name in arch docker docker_compose go homebrew java nmap node perl python rsync ruby
        source $dir'/alias_def_'$name'.fish'
    end
end
