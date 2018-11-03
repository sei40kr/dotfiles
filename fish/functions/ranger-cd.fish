# ranger-cd.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function ranger-cd -w 'ranger' -d 'Automatically change the directory in fish after closing ranger'
    set -l tempfile (mktemp -t tmp.XXXXXX)

    ranger --choosedir=$tempfile $argv

    if [ -f $tempfile ]
        cd (cat $tempfile)
    end
    rm -f $tempfile
end
