# 50_gem.pl --- gem facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @gem_install_intermediate = ();

sub gem_install {
    my $gem = $_[0];

    push( @gem_install_intermediate, $gem );
}

my sub gem_install_reducer {
    return if ( scalar(@gem_install_intermediate) eq 0 );

    is_exec('gem') or error('gem not found.');

    log_wait('Installing Rubygems ...');

    my @cmd = qw(gem install -q --silent);
    push( @cmd, qw(--conservative --minimal-deps) ) unless (&do_update);
    run_cmd(@cmd, @gem_install_intermediate);
}

register_reducer(\&gem_install_reducer);

1;
