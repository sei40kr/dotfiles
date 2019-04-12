# capit.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function capit -w cap
    if [ -f Gemfile ]
        bundle exec cap $argv
    else
        cap $argv
    end
end
