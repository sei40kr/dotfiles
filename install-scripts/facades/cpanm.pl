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

my sub cpanm_reducer {
    return if ( scalar(@cpanm_intermediate) eq 0 );

    error('perlbrew not found.')
      unless ( -x "${ENV{PERLBREW_ROOT}}/bin/perlbrew" or &is_dry_run );
    my $cpanm = "${ENV{PERLBREW_ROOT}}/bin/cpanm";
    error('cpanm not found.') unless ( -x $cpanm or &is_dry_run );

    log_wait('Installing CPAN modules ...');

    Command::run( $cpanm, '-q', @cpanm_intermediate );
}

register_reducer( 20, \&cpanm_reducer );

1;
