{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vscode;
  isDarwin = pkgs.stdenv.isDarwin;
in {
  options.modules.editors.vscode = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.vscode = {
      enable = true;
      extensions = with pkgs;
        [
          vscode-extensions.alanz.vscode-hie-server
          vscode-extensions.coenraads.bracket-pair-colorizer-2
          vscode-extensions.dbaeumer.vscode-eslint
          # vscode-extensions.editorconfig.editorconfig
          vscode-extensions.esbenp.prettier-vscode
          vscode-extensions.github.vscode-pull-request-github
          vscode-extensions.golang.Go
          vscode-extensions.mechatroner.rainbow-csv
          vscode-extensions.redhat.vscode-yaml
          vscode-extensions.scala-lang.scala
          vscode-extensions.scalameta.metals
          my.vscode-extensions.Pivotal.vscode-boot-dev-pack
          my.vscode-extensions.REditorSupport.r-lsp
          my.vscode-extensions.alefragnani.Bookmarks
          my.vscode-extensions.castwide.solargraph
          my.vscode-extensions.ccls-project.ccls
          my.vscode-extensions.eamodio.gitlens
          my.vscode-extensions.felixfbecker.php-intellisense
          my.vscode-extensions.rust-lang.rust
          my.vscode-extensions.vscjava.vscode-java-pack
          my.vscode-extensions.vscodevim.vim
        ] ++ (optionals (!isDarwin) [
          vscode-extensions.ms-python.python
          vscode-extensions.ms-vscode.cpptools
        ]);
      package = pkgs.vscode;
    };
    home.file = mkIf isDarwin {
      "Library/Application Support/VSCode/User/keybindings.json".source =
        "${configDir}/vscode/keybindings.json";
      "Library/Application Support/VSCode/User/settings.json".source =
        "${configDir}/vscode/settings.json";
    };
    home.configFile = mkIf (!isDarwin) {
      "VSCode/User/keybindings.json".source =
        "${configDir}/vscode/keybindings.json";
      "VSCode/User/settings.json".source = "${configDir}/vscode/settings.json";
    };
    modules.editors.fonts.enable = true;
  };
}
