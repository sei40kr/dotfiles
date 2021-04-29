{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.shell.xonsh.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.xonsh.enable {
    user.packages = with pkgs; [ xonsh ];
    home.file.".xonshrc".source = "${configDir}/xonsh/xonshrc";
  };
}
