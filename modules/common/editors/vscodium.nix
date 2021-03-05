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
      extensions = with pkgs;
        with pkgs.my; [
          vscode-extensions.alanz.vscode-hie-server
          vscode-extensions.alefragnani.Bookmarks
          vscode-extensions.castwide.solargraph
          vscode-extensions.ccls-project.ccls
          vscode-extensions.CoenraadS.bracket-pair-colorizer-2
          vscode-extensions.dbaeumer.vscode-eslint
          vscode-extensions.eamodio.gitlens
          vscode-extensions.EditorConfig.EditorConfig
          vscode-extensions.esbenp.prettier-vscode
          vscode-extensions.felixfbecker.php-intellisense
          vscode-extensions.GitHub.vscode-pull-request-github
          vscode-extensions.golang.Go
          vscode-extensions.mechatroner.rainbow-csv
          vscode-extensions.ms-python.python
          vscode-extensions.ms-vscode.cpptools
          vscode-extensions.Pivotal.vscode-boot-dev-pack
          vscode-extensions.redhat.vscode-yaml
          vscode-extensions.REditorSupport.r-lsp
          vscode-extensions.rust-lang.rust
          vscode-extensions.scala-lang.scala
          vscode-extensions.scalameta.metals
          vscode-extensions.vscjava.vscode-java-pack
          vscode-extensions.vscodevim.vim
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
