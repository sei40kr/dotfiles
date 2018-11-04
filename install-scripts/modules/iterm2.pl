# iterm2.pl --- iTerm2 installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_macos) {
    brew_cask_install('iterm2');

    ln( dotfile('iterm2'), "${ENV{HOME}}/iterm2_profile" );

    # Download imgcat
    curl(
        'https://www.iterm2.com/utilities/imgcat',
        "${ENV{HOME}}/.local/bin/imgcat"
    );
    chmod_facade( 755, "${ENV{HOME}}/.local/bin/imgcat" );

    # Specify the preferences directory
    defaults_write_string( 'com.googlecode.iterm2.plist', 'PrefsCustomFolder',
        "${ENV{HOME}}/iterm2_profile" );

    # Tell iTerm2 to use the custom preferences in the directory
    defaults_write_bool( 'com.googlecode.iterm2.plist',
        'LoadPrefsFromCustomFolder', 1 );
}

1;
