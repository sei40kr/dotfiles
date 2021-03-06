{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.shell = with types; {
    aliases = mkOpt attrs { };
    env = mkOpt attrs { };
  };

  config = {
    modules.shell = {
      env = { PROJECT_DIR = "${homeDir}/projects"; };
      aliases = {
        u = "cd ..";
        cx = "chmod +x";

        md = "mkdir -p";
        rd = "rmdir";
        sortnr = "sort -nr";
      };
    };
  };
}
