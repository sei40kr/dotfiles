{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.xonsh.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.xonsh.enable {
    my.packages = with pkgs; [ xonsh ];
    my.home.home.file.".xonshrc".source = <config/xonsh/xonshrc>;
  };
}
