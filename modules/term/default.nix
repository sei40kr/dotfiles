{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term;
  fontType = with types;
    submodule {
      options = {
        package = mkOpt (nullOr package) null;
        name = mkOpt str null;
        size = mkOpt int null;
      };
    };
in {
  options.modules.term = with types; {
    font = mkOpt fontType {
      name = "monospace";
      size = 12;
    };
  };

  config = {
    user.packages = optionals (cfg.font.package != null) [ cfg.font.package ];
  };
}
