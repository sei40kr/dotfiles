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

my sub install_nvm {
    log_wait('Installing nvm ...');

    git_clone_internal( 'https://github.com/creationix/nvm.git',
        'master', "${ENV{NVM_DIR}}" );
}

my sub install_node {
    log_wait('Installing Node ...');

    run_cmd( qw(sh -c),
        ". ${ENV{NVM_DIR}}/nvm.sh && nvm install --no-progress stable" );
    run_cmd( qw(sh -c),
        ". ${ENV{NVM_DIR}}/nvm.sh && nvm alias default stable" );
    run_cmd(
        qw(sh -c),
"export NVM_SYMLINK_CURRENT=true && . ${ENV{NVM_DIR}}/nvm.sh && nvm use --delete-prefix default"
    );
}

my sub install_yarn_or_err {
    log_wait('Installing Yarn ...');

    if (&is_macos) {
        run_cmd(qw(brew install yarn --without-node));
    }
    elsif (&is_arch) {
        run_cmd(
            qw(sudo pacman -S --needed --noconfirm --noprogressbar --assume-installed nodejs yarn)
        );
    }
    else {
        error(
"Unable to install Yarn on your OS automatically. Please install Yarn manually."
        );
    }
}

my sub yarn_global_add_reducer {
    return if ( scalar(@yarn_global_add_intermediate) eq 0 );

    &install_nvm unless ( -x "${ENV{NVM_DIR}}/nvm.sh" );
    my $current_node = "${ENV{NVM_DIR}}/current/bin/node";
    &install_node unless ( -x $current_node );
    &install_yarn_or_err unless ( is_exec('yarn') );

    log_wait('Installing Yarn packages ...');

    error('node not found.') unless ( -x $current_node or &is_dry_run );

    # TODO Use the Node.js installed via nvm
    my @cmd = qw(yarn global add -s --noprogress --non-interactive);
    push( @cmd, '--latest' ) if (&do_update);
    run_cmd( @cmd, @yarn_global_add_intermediate );
}

register_reducer( \&yarn_global_add_reducer );

1;
