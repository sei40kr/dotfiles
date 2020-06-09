{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.circleciCli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.circleciCli.enable {
    my.packages = with pkgs; [ circleci-cli ];
  };
}
