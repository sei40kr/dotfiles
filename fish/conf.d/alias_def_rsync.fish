# alias_def_rsync.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# rsync
alias rsync-copy 'rsync -avz --progress -h'
alias rsync-move 'rsync -avz --progress -h --remove-source-files'
alias rsync-update 'rsync -avzu --progress -h'
alias rsync-synchronize 'rsync -avzu --delete --progress -h'
