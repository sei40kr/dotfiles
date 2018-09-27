# 50_yarn.pl --- yarn facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @yarn_global_add_intermediate = ();

sub yarn_global_add {
    my $pkg = $_[0];

    push( @yarn_global_add_intermediate, $pkg );
}

my sub yarn_global_add_reducer {
    return if ( scalar(@yarn_global_add_intermediate) eq 0 );

    is_exec('yarn') or error('yarn not found.');

    log_wait('Installing Yarn packages ...');

    my @cmd = qw(yarn global add -s --noprogress --non-interactive);
    push( @cmd, '--latest' ) if (&do_update);
    run_cmd(@cmd, @yarn_global_add_intermediate);
}

register_reducer( \&yarn_global_add_reducer );

1;
