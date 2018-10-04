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

my sub install_rbenv {
    # TODO Update rbenv itself when option --update given
    log_wait('Installing rbenv ...');

    git_clone_internal( 'https://github.com/rbenv/rbenv.git',
        'master', "${ENV{RBENV_ROOT}}" );
    git_clone_internal( 'https://github.com/rbenv/ruby-build.git',
        'master', "${ENV{RBENV_ROOT}}/plugins/ruby-build" );
}

my sub ruby_stable_version {
    my $rbenv_proc;
    my $stable_version;

    # TODO Think about a case when --dry-run and --update options given
    open $rbenv_proc, '-|', qw(rbenv install -l);
    while (<$rbenv_proc>) {
        $stable_version = $1 if ( $_ =~ /^\s*((?:\d+\.){2}\d+)$/ );
    }
    close $rbenv_proc;

    return $stable_version;
}

my sub install_ruby {
    my $stable_version = &ruby_stable_version;

    log_wait("Installing Ruby ${stable_version} ...");

    run_cmd( "${ENV{RBENV_ROOT}}/bin/rbenv", qw(install -s), $stable_version );
    run_cmd( "${ENV{RBENV_ROOT}}/bin/rbenv", 'global',       $stable_version );
}

my sub gem_install_reducer {
    return if ( scalar(@gem_install_intermediate) eq 0 );

    &install_rbenv unless ( -x "${ENV{RBENV_ROOT}}/bin/rbenv" );
    my $gem = "${ENV{RBENV_ROOT}}/shims/gem";
    &install_ruby if ( !-x $gem or &do_update );

    log_wait('Installing Rubygems ...');

    error('gem not found.') unless ( -x $gem );

    my @gem_args = qw(install -q --silent);
    push( @gem_args, qw(--conservative --minimal-deps) ) unless (&do_update);
    run_cmd( $gem, @gem_args, @gem_install_intermediate );
}

register_reducer( \&gem_install_reducer );

1;
