# 50_gem.pl --- gem facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::CommandRunner;

my @gem_install_intermediate = ();

sub gem_install {
    my $gem = $_[0];

    push( @gem_install_intermediate, $gem );
}

my sub gem_install_reducer {
    return if ( scalar(@gem_install_intermediate) eq 0 );

    &install_rbenv unless ( -x "${ENV{RBENV_ROOT}}/bin/rbenv" );
    my $gem = "${ENV{RBENV_ROOT}}/shims/gem";
    &install_ruby if ( !-x $gem or &do_update );

    log_wait('Installing Rubygems ...');

    error('gem not found.') unless ( -x $gem or &is_dry_run );

    my @gem_args = qw(install -q --silent --norc);
    push( @gem_args, qw(--conservative --minimal-deps) ) unless (&do_update);
    run( $gem, @gem_args, @gem_install_intermediate );
}

register_reducer( 61, \&gem_install_reducer );

1;
