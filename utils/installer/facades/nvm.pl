# nvm.pl --- Node Version Manager facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @nvm_install_intermediate = ();
my %nvm_alias_intermediate   = ();

sub nvm_install {
    my $version = $_[0];

    push( @nvm_install_intermediate, $version );
}

sub nvm_alias {
    my ( $name, $version ) = @_;

    $nvm_alias_intermediate{$name} = $version;
}

my sub find_nvm_exec {
    foreach my $dirpath ("${ENV{HOME}}/.nvm") {
        return "${dirpath}/nvm-exec" if ( -x "${dirpath}/nvm-exec" );
    }
}

my sub nvm_install_reducer {
    return if ( scalar(@nvm_install_intermediate) eq 0 );

    my $nvm_exec = &find_nvm_exec;
    error('nvm is not installed.') unless ( defined($nvm_exec) );

    log_wait('Installing Node ...');

    log_warn(
"nvm can't skip checking updates for these Node versions: stable, unstable."
    ) if ( grep( /^stable|unstable$/, @nvm_install_intermediate ) );

    Command::run( $nvm_exec, 'install', '--no-progress', $_ )
      foreach @nvm_install_intermediate;
}

my sub nvm_alias_reducer {
    return if ( scalar( keys %nvm_alias_intermediate ) eq 0 );

    my $nvm_exec = &find_nvm_exec;
    error('nvm is not installed.') unless ( defined($nvm_exec) );

    log_wait('Set aliases pointing to Node versions ...');

    foreach my $name ( keys %nvm_alias_intermediate ) {
        Command::run( $nvm_exec, 'alias', $name,
            $nvm_alias_intermediate{$name} );
    }
}

register_reducer( 50, \&nvm_install_reducer );
register_reducer( 51, \&nvm_alias_reducer );

1;
