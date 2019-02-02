# 10_brew.pl --- brew facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;

my @brew_tap_intermediate          = ();
my @brew_install_intermediate      = ();
my @brew_cask_install_intermediate = ();

sub brew_tap {
    my ( $user_and_repo, $url ) = @_;

    push(
        @brew_tap_intermediate,
        {
            user_and_repo => $user_and_repo,
            url           => $url
        }
    );
}

sub brew_install {
    my ( $formula, @install_opts ) = @_;

    push(
        @brew_install_intermediate,
        {
            formula      => $formula,
            install_opts => \@install_opts
        }
    );
}

sub brew_cask_install {
    my $cask = $_[0];

    push( @brew_cask_install_intermediate, $cask );
}

my sub generate_brewfile {
    my $s = '';

    foreach my $item (@brew_tap_intermediate) {
        $s .= sprintf( "tap \"%s\"", $item->{user_and_repo} );
        $s .= sprintf( ", \"%s\"",   $item->{url} )
          if ( defined( $item->{url} ) );
        $s .= "\n";
    }

    foreach my $item (@brew_install_intermediate) {
        $s .= sprintf( "brew \"%s\", args: [%s]\n",
            $item->{formula},
            join( ', ', map { "'$_'" } @{ $item->{install_opts} } ) );
    }

    $s .= "cask_args appdir: \"~/Applications\"\n"
      if ( scalar(@brew_cask_install_intermediate) ne 0 );
    $s .= "cask \"${_}\"\n" foreach @brew_cask_install_intermediate;

    return $s;
}

my sub install_homebrew {
    log_wait('Installing Homebrew ...');

    # TODO
}

sub brew_reducer {
    return
      if (  scalar(@brew_tap_intermediate) eq 0
        and scalar(@brew_install_intermediate) eq 0
        and scalar(@brew_cask_install_intermediate) eq 0 );

    my @command = qw( brew bundle --file=- );
    unless (&do_update) {
        push( @command, "--no-upgrade" );
    }

    &install_homebrew unless ( is_exec('brew') );

    log_wait('Installing Homebrew repos, formulas, casks ...');

    my $brewfile = &generate_brewfile;
    run_with_stdin($brewfile, @command)
}

register_reducer( 40, \&brew_reducer );

1;