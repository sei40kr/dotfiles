{ lib, pkgs, ... }:

with lib;
with lib.my; {
  config = {
    user.packages = with pkgs; [
      coreutils
      diffutils
      findutils
      gnugrep
      gnumake
      gnutar
      gnused
      gzip
      libtool
    ];

    modules.shell.zsh.zinit.plugins = {
      source = "${pkgs.zsh-prezto}/modules/gnu-utility";
      ice.id-as = "PZT::modules--gnu-utility";
    };
  };
}
