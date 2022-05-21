{ lib, ... }:

with lib;
with lib.my; {
  options.modules.shell = with types; {
    aliases = mkOpt attrs { };
    env = mkOpt attrs { };
  };

  config = {
    modules.shell.aliases.u = "cd ..";
  };
}
