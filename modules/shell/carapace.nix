{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.shell.carapace;

  carapace_nu = pkgs.runCommandLocal "carapace.nu" { buildInputs = [ pkgs.carapace ]; } ''
    carapace _carapace nushell >$out
  '';
in
{
  options.modules.shell.carapace = {
    enable = mkEnableOption "Carapace";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ carapace ];

    modules.shell.nushell.rcInit = ''
      source ${carapace_nu}
    '';
  };
}
