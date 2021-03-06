{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.vscodium;
in {
  options.modules.editors.vscodium = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions;
        with pkgs.my.vscode-extensions; [
          alanz.vscode-hie-server
          alefragnani.Bookmarks
          castwide.solargraph
          ccls-project.ccls
          CoenraadS.bracket-pair-colorizer-2
          dbaeumer.vscode-eslint
          eamodio.gitlens
          EditorConfig.EditorConfig
          esbenp.prettier-vscode
          felixfbecker.php-intellisense
          GitHub.vscode-pull-request-github
          golang.Go
          mechatroner.rainbow-csv
          ms-python.python
          ms-vscode.cpptools
          Pivotal.vscode-boot-dev-pack
          redhat.vscode-yaml
          REditorSupport.r-lsp
          rust-lang.rust
          scala-lang.scala
          scalameta.metals
          vscjava.vscode-java-pack
          vscodevim.vim
        ];
    };
    home.configFile = {
      "VSCodium/User/keybindings.json".source =
        "${configDir}/vscodium/keybindings.json";
      "VSCodium/User/settings.json".source =
        "${configDir}/vscodium/settings.json";
    };
    modules.editors.fonts.enable = true;
  };
}
