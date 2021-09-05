{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.shell = with types; {
    aliases = mkOpt attrs { };
    env = mkOpt attrs { };
  };

  config = {
    modules.shell = {
      env.WORKSPACE_DIR = "${config.user.home}/dev/ws";
      aliases.u = "cd ..";
    };
  };
}
