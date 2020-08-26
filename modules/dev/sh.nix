{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.sh.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.sh.enable {
    modules.dev.editors.tools.packages = with pkgs;
      with pkgs.nodePackages; [
        bash-language-server
        shellcheck
        shfmt
      ];

    my.packages = with pkgs; [ shellcheck shfmt ];
  };
}
