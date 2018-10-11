use utf8;
use strict;
use warnings;

# ripgrep.pl --- ripgrep installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if (&is_macos) {
    brew_tap( 'burntsushi/ripgrep',
        'https://github.com/BurntSushi/ripgrep.git' );
    brew_install('ripgrep-bin');

    ln(
"${ENV{BREW_PREFIX}}/opt/ripgrep-bin/share/fish/vendor_completions.d/rg.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/completions/rg.fish"
    );
}
elsif (&is_arch) {
    pacman_sync('ripgrep');
}

if ( &is_macos or &is_arch ) {
    ln( 'ripgrep/ripgreprc', "${ENV{HOME}}/.ripgreprc" );
}

1;
