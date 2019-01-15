# atcoder.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

# cf https://language-test-201603.contest.atcoder.jp

# Go
goenv_install('1.6');

# Node
nvm_install('v5.12.0');

# Python
pyenv_install('3.4.3');

# Ruby
rbenv_install('2.3.3');

# Rust
rustup_toolchain_install('1.15.1');
rustup_component_add('rls-preview', '1.15.1');
rustup_component_add('rust-analysis', '1.15.1');
rustup_component_add('rust-src', '1.15.1');

1;
