{ config, home-manager, lib, options, ... }:

with lib;
with lib.my; {
  options.modules.desktop = with types; {
    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (_: v:
        if isList v then concatMapStringsSep ":" toString v else (toString v));
      default = { };
    };
  };

  config = {
    modules.desktop.env = { GIO_EXTRA_MODULES = [ "\${GIO_EXTRA_MODULES}" ]; };
  };
}
