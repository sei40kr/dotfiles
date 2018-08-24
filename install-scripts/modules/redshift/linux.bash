# linux.bash --- Redshift installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades symlink

symlink_make redshift/redshift.conf .config/redshift/redshift.conf
symlink_make redshift/hooks/brightness.sh .config/redshift/hooks/brightness.sh
