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

    log_wait('Installing Yarn packages ...');

    error('yarn is not installed.') unless is_exec('yarn');

    # TODO Use the Node.js installed via nvm
    my @cmd =
      qw(yarn global add --no-default-rc --noprogress --non-interactive);
    push( @cmd, '--latest' ) if (&do_update);
    Command::run( @cmd, @yarn_global_add_intermediate );
}

register_reducer( 61, \&yarn_global_add_reducer );

1;
