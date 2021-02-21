{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.jq.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.jq.enable {
    user.packages = with pkgs; [ jq ];
  };
}
