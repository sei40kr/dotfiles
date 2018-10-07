# balias_def_rsync.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# rsync
balias rsync-copy 'rsync -avz --progress -h'
balias rsync-move 'rsync -avz --progress -h --remove-source-files'
balias rsync-update 'rsync -avzu --progress -h'
balias rsync-synchronize 'rsync -avzu --delete --progress -h'
