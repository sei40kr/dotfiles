{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.web;
in
{
  options.modules.dev.lang.web = {
    enable = mkEnableOption "Web development";

    bun.enable = mkEnableOption "Bun";

    deno.enable = mkEnableOption "Deno";
  };

  config = mkIf cfg.enable {
    # TODO stylelint-cli
    environment.systemPackages = with pkgs; [
      nodejs_20
      emmet-language-server
      nodePackages.prettier
      nodePackages.vscode-langservers-extracted
      nodePackages.vue-language-server
      vtsls
      (mkIf cfg.bun.enable bun)
      (mkIf cfg.deno.enable deno)
    ];
  };
}
