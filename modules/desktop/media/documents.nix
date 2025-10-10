{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.desktop.media.documents;
in
{
  options.modules.desktop.media.documents = {
    pdf.enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [ (mkIf cfg.pdf.enable zathura) ];
  };
}
