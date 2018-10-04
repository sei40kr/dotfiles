# 50_pip.pl --- pip facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @pip2_install_intermediate = ();
my @pip3_install_intermediate = ();

sub pip2_install {
    my $pkg = $_[0];

    push( @pip2_install_intermediate, $pkg );
}

sub pip3_install {
    my $pkg = $_[0];

    push( @pip3_install_intermediate, $pkg );
}

my sub install_pyenv {
    log_wait('Installing pyenv ...');

    # TODO Update pyenv itself when option --update given
    git_clone_internal( 'https://github.com/pyenv/pyenv.git',
        'master', "${ENV{PYENV_ROOT}}" );
    git_clone_internal( 'https://github.com/pyenv/pyenv-virtualenv.git',
        'master', "${ENV{PYENV_ROOT}}/plugins/pyenv-virtualenv" );
}

my sub python_stable_versions {
    my $pyenv_proc;
    my $py2_stable_ver;
    my $py3_stable_ver;

    # TODO Think about a case when --dry-run and --update options given
    open $pyenv_proc, '-|', "${ENV{PYENV_ROOT}}/bin/pyenv", qw(install -l);
    while (<$pyenv_proc>) {
        if ( $_ =~ /^\s*(2(?:\.\d+){2})$/ ) {
            $py2_stable_ver = $1;
        }
        elsif ( $_ =~ /^\s*(3(?:\.\d+){2})$/ ) {
            $py3_stable_ver = $1;
        }
    }
    close $pyenv_proc;

    return ( $py2_stable_ver, $py3_stable_ver );
}

my sub install_py2_and_py3 {
    my $pyenv = "${ENV{PYENV_ROOT}}/bin/pyenv";
    my ( $py2_stable_ver, $py3_stable_ver ) = &python_stable_versions;

    log_wait("Installing Python ${py2_stable_ver} ...");
    run_cmd( $pyenv, qw(install -s), $py2_stable_ver );

    log_wait("Installing Python ${py3_stable_ver} ...");
    run_cmd( $pyenv, qw(install -s), $py3_stable_ver );

    run_cmd( $pyenv, 'global', $py2_stable_ver, $py3_stable_ver );
}

# A dummy reducer to install pyenv, Python 2/3
my sub dummy_reducer {
    return
      if (  scalar(@pip2_install_intermediate) eq 0
        and scalar(@pip3_install_intermediate) eq 0 );

    &install_pyenv
      unless ( -x "${ENV{PYENV_ROOT}}/bin/pyenv" );
    my $shims = "${ENV{PYENV_ROOT}}/shims";
    &install_py2_and_py3
      if ( !-x "${shims}/pip2" or !-x "${shims}/pip3" or &do_update );
}

my sub pip2_install_reducer {
    return if ( scalar(@pip2_install_intermediate) eq 0 );

    log_wait('Installing Python2 packages ...');

    my $pip2 = "${ENV{PYENV_ROOT}}/shims/pip2";
    error('pip2 not found.') unless ( -x $pip2 );

    my @pip2_args = ('install');
    push( @pip2_args, '-U' ) if (&do_update);
    run_cmd( $pip2, @pip2_args, @pip2_install_intermediate );
}

my sub pip3_install_reducer {
    return if ( scalar(@pip3_install_intermediate) eq 0 );

    log_wait('Installing Python3 packages ...');

    my $pip3 = "${ENV{PYENV_ROOT}}/shims/pip3";
    error('pip3 not found.') unless ( -x $pip3 );

    my @pip3_args = ('install');
    push( @pip3_args, '-U' ) if (&do_update);
    run_cmd( $pip3, @pip3_args, @pip3_install_intermediate );
}

register_reducer( \&dummy_reducer );
register_reducer( \&pip2_install_reducer );
register_reducer( \&pip3_install_reducer );
