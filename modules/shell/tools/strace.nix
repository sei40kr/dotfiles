{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.strace.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.strace.enable {
    user.packages = with pkgs; [ strace ];
  };
}
