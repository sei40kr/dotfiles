{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.awsShell.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.awsShell.enable {
    my.packages = with pkgs; [ aws_shell ];
  };
}
