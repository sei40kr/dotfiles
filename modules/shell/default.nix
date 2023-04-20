{ lib, ... }:

with lib;
with lib.my; {
  options.modules.shell = with types; {
    aliases = mkOpt (attrsOf (nullOr (either str path))) { };
    env = mkOpt attrs { };
  };

  config = {
    modules.shell.aliases.u = "cd ..";
  };
}
