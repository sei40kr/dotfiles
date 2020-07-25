{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.editors.vscodium.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.vscodium.enable {
    modules.dev.editors = {
      fonts.enable = mkForce true;
      tabnine.enable = mkForce true;
    };

    my.home.programs.vscode = {
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
          TabNine.tabnine-vscode
          vscjava.vscode-java-pack
          vscodevim.vim
        ];
    };
    my.home.xdg.configFile = {
      "VSCodium/User/keybindings.json".source =
        <config/vscodium/keybindings.json>;
      "VSCodium/User/settings.json".source = <config/vscodium/settings.json>;
    };
  };
}
