{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./desktop ./dev ./shell ];

  options.my = {
    env = mkOption {
      type = with types;
        attrsOf (either (either str path) (listOf (either str path)));
      apply = mapAttrs (n: v:
        if isList v then
          concatMapStringsSep ":" (x: toString x) v
        else
          (toString v));
    };
    packages = mkOption { type = with types; listOf package; };
  };

  config = {
    my.env = { PATH = [ "$PATH" ]; };

    home = {
      packages = config.my.packages;
      sessionVariables = config.my.env;
    };
  };
}
