# alias_def_rsync.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# rsync
alias -s rsync-copy 'rsync -avz --progress -h'
alias -s rsync-move 'rsync -avz --progress -h --remove-source-files'
alias -s rsync-update 'rsync -avzu --progress -h'
alias -s rsync-synchronize 'rsync -avzu --delete --progress -h'
