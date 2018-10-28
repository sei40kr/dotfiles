# 50_go.pl --- go facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @go_get_intermediate = ();

sub go_get {
    my $pkg = $_[0];

    push( @go_get_intermediate, $pkg );
}

my sub install_goenv {

    # TODO Update goenv itself when option --update given
    log_wait('Installing goenv ...');

    git_clone_internal( 'https://github.com/syndbg/goenv.git',
        'master', "${ENV{GOENV_ROOT}}" );
}

my sub go_stable_version {
    my $goenv_proc;
    my $stable_version;

    open $goenv_proc, '-|', "${ENV{GOENV_ROOT}}/bin/goenv", qw(install -l);
    while (<$goenv_proc>) {
        $stable_version = $1 if ( $_ =~ /^\s*((?:\d+\.){2}\d+)$/ );
    }
    close $goenv_proc;

    return $stable_version;
}

my sub install_go {
    my $stable_version = &go_stable_version;

    log_wait("Installing Go ${stable_version} ...");

    run_cmd( "${ENV{GOENV_ROOT}}/bin/goenv", qw(install -s), $stable_version );
    run_cmd( "${ENV{GOENV_ROOT}}/bin/goenv", 'global',       $stable_version );
}

my sub go_get_reducer {
    return if ( scalar(@go_get_intermediate) eq 0 );

    &install_goenv unless ( -x "${ENV{GOENV_ROOT}}/bin/goenv" );
    my $go = "${ENV{GOENV_ROOT}}/shims/go";
    &install_go if ( !-x $go or &do_update );

    log_wait('Installing Go packages ...');

    error('go not found.') unless ( -x $go or &is_dry_run );

    my @go_args = ('get');
    push( @go_args, '-u' ) if (&do_update);
    run_cmd( 'go', @go_args, @go_get_intermediate );
}

register_reducer( 20, \&go_get_reducer );

1;
