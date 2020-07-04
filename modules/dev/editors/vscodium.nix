{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./fonts.nix ];

  options.modules.dev.editors.vscodium.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.vscodium.enable {
    my.home.programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions;
        with pkgs.my.vscode-extensions; [
          alefragnani.Bookmarks
          CoenraadS.bracket-pair-colorizer-2
          dbaeumer.vscode-eslint
          eamodio.gitlens
          esbenp.prettier-vscode
          EditorConfig.EditorConfig
          GitHub.vscode-pull-request-github
          mechatroner.rainbow-csv
          vscodevim.vim
        ];
    };
    my.home.xdg.configFile."VSCodium/User/settings.json".source =
      <config/vscodium/settings.json>;
  };
}
