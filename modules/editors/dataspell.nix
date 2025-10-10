{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.editors.dataspell;
in
{
  options.modules.editors.dataspell = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ jetbrains.dataspell ];

    modules.editors = {
      fonts.enable = true;
      ideavim.enable = true;
    };
  };
}
