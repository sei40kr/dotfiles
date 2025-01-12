{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.astro;
in
{
  options.modules.dev.lang.astro = {
    enable = mkEnableOption "Astro";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ astro-language-server ];
  };
}
