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

my sub install_yarn_or_err {
    if (&is_macos) {
        run_cmd(qw(brew install yarn --without-node));
    }
    elsif (&is_arch) {
        run_cmd(
            qw(sudo pacman -S --needed --noconfirm --noprogressbar --assume-installed nodejs yarn)
        );
    }
    else {
        error('yarn not found.');
    }
}

my sub yarn_global_add_reducer {
    return if ( scalar(@yarn_global_add_intermediate) eq 0 );

    is_exec('yarn') or &install_yarn_or_err;

    log_wait('Installing Yarn packages ...');

    my @cmd = qw(yarn global add -s --noprogress --non-interactive);
    push( @cmd, '--latest' ) if (&do_update);
    run_cmd( @cmd, @yarn_global_add_intermediate );
}

register_reducer( \&yarn_global_add_reducer );

1;
