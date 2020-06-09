{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.git.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.git.enable {
    my.packages = [ pkgs.git ];

    my.home.xdg.configFile = {
      "git/config".source = <config/git/config>;
      "git/ignore".source = <config/git/ignore>;
    };
  };
}
