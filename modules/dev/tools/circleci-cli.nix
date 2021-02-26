{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.circleciCli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.circleciCli.enable {
    user.packages = with pkgs; [ circleci-cli ];
  };
}
