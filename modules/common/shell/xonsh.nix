{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.xonsh;
in {
  options.modules.shell.xonsh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ xonsh ];
    home.file.".xonshrc".source = "${configDir}/xonsh/xonshrc";
  };
}
