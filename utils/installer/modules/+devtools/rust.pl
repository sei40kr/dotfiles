# rust.pl --- Rust installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

rustup_toolchain_install('stable');
rustup_toolchain_install('nightly');

1;
