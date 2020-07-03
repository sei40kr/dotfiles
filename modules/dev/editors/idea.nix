{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./fonts.nix ];

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
      source ${../../../idea-doom-emacs/ideavimrc}

      " Do not exit visual mode on a selection shift
      vnoremap < <gv
      vnoremap > >gv
    '';
  });
}
