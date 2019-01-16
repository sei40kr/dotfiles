# leetcode.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

# Rust
rustup_toolchain_install('1.31.0');
rustup_component_add( 'rls-preview',   '1.31.0' );
rustup_component_add( 'rust-analysis', '1.31.0' );
rustup_component_add( 'rust-src',      '1.31.0' );

1;
