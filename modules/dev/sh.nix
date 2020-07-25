{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.sh.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.sh.enable {
    # TODO Install bash-language-server
    my.packages = with pkgs; [ shellcheck shfmt ];
  };
}
