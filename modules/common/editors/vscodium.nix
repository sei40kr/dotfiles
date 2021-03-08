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
      extensions = with pkgs; [
        vscode-extensions.alanz.vscode-hie-server
        my.vscode-extensions.alefragnani.Bookmarks
        my.vscode-extensions.castwide.solargraph
        my.vscode-extensions.ccls-project.ccls
        my.vscode-extensions.CoenraadS.bracket-pair-colorizer-2
        my.vscode-extensions.dbaeumer.vscode-eslint
        my.vscode-extensions.eamodio.gitlens
        my.vscode-extensions.EditorConfig.EditorConfig
        my.vscode-extensions.esbenp.prettier-vscode
        my.vscode-extensions.felixfbecker.php-intellisense
        my.vscode-extensions.GitHub.vscode-pull-request-github
        my.vscode-extensions.golang.Go
        my.vscode-extensions.mechatroner.rainbow-csv
        vscode-extensions.ms-python.python
        vscode-extensions.ms-vscode.cpptools
        my.vscode-extensions.Pivotal.vscode-boot-dev-pack
        my.vscode-extensions.redhat.vscode-yaml
        my.vscode-extensions.REditorSupport.r-lsp
        my.vscode-extensions.rust-lang.rust
        vscode-extensions.scala-lang.scala
        vscode-extensions.scalameta.metals
        my.vscode-extensions.vscjava.vscode-java-pack
        my.vscode-extensions.vscodevim.vim
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
