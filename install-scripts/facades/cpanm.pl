# 50_cpanm.pl --- cpanm facade
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

my @cpanm_intermediate = ();

sub cpanm {
    my $module = $_[0];

    push( @cpanm_intermediate, $module );
}

my sub find_cpanm_exec {
    foreach my $dirpath ( "${ENV{HOME}}/perl5/perlbrew/bin", "/usr/local/bin",
        "/usr/bin" )
    {
        return "${dirpath}/cpanm" if ( -x "${dirpath}/cpanm" );
    }
}

my sub cpanm_reducer {
    return if ( scalar(@cpanm_intermediate) eq 0 );

    log_wait('Installing CPAN modules ...');

    my $cpanm_exec = &find_cpanm_exec;
    error('cpanm is not installed.') unless ( defined($cpanm_exec) );

    log_warn("cpanm can't skip checking updates.") unless (&do_update);

    Command::run( $cpanm_exec, '-q', @cpanm_intermediate );
}

register_reducer( 61, \&cpanm_reducer );

1;
