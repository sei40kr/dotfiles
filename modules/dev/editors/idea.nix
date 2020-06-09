{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.editors.idea.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.idea.enable (let
    intellimacs = builtins.fetchGit ({
      url = "https://github.com/MarcoIeni/intellimacs.git";
    });
  in {
    my.packages = with pkgs; [ jetbrains.idea-ultimate ];

    my.home.home.file.".ideavimrc".text = ''
      source ${intellimacs.outPath}/spacemacs/buffers.vim
      source ${intellimacs.outPath}/spacemacs/help.vim
      source ${intellimacs.outPath}/spacemacs/misc.vim
      source ${intellimacs.outPath}/spacemacs/search-symbol.vim
      source ${intellimacs.outPath}/hybrid.vim

      source ${<config/idea/extra.vimrc>}
    '';
  });
}
