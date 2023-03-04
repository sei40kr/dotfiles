{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.documents;
in
{
  options.modules.desktop.media.documents = {
    pdf.enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [ (mkIf cfg.pdf.enable zathura) ];
  };
}
