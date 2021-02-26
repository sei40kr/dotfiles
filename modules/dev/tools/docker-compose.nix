{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.dockerCompose.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.dockerCompose.enable {
    user.packages = with pkgs; [ docker-compose ];
  };
}
