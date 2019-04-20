# competitive-programming.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;


# install AtCoder specific versions of environments
# cf https://language-test-201603.contest.atcoder.jp
goenv_install('1.6.0');
nvm_install('v5.12.0');
pyenv_install('3.4.3');
rbenv_install('2.3.3');
rustup_toolchain_install('1.15.1');
rustup_component_add( 'rust-src', '1.15.1' );

# rls for 1.15.1 does not exist
# rustup_component_add( 'rls-preview',   '1.15.1' );
# rustup_component_add( 'rust-analysis', '1.15.1' );

# install LeetCode specific version of environment
rustup_toolchain_install('1.31.0');
rustup_component_add( 'rls-preview',   '1.31.0' );
rustup_component_add( 'rust-analysis', '1.31.0' );
rustup_component_add( 'rust-src',      '1.31.0' );

# install atcoder-tools
pip3_install('atcoder-tools');
ln(
    dotfile('atcoder-tools/atcodertools.toml'),
    "${ENV{HOME}}/.atcodertools.toml"
);
ln( dotfile('atcoder-tools/my_template.rs'), "${ENV{HOME}}/my_template.rs" );
ln(
    dotfile('atcoder-tools/custom_code_generator.py'),
    "${ENV{HOME}}/custom_code_generator.py"
);

# install online-judge-tools
pip3_install('online-judge-tools');

1;
