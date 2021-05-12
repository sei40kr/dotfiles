{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.ideavim;
in {
  options.modules.editors.ideavim = {
    enable = mkBoolOpt false;
    enableDoom = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.file.".ideavimrc".text =
      (let idea-doom-emacs = pkgs.my.idea-doom-emacs;
      in optionalString cfg.enableDoom ''
        source ${idea-doom-emacs}/share/vim-plugins/idea-doom-emacs/ideavimrc
        source ${idea-doom-emacs}/share/vim-plugins/idea-doom-emacs/expand-region.vim
      '') + ''

        " Do not exit visual mode on a selection shift
        vnoremap < <gv
        vnoremap > >gv
      '';
  };
}
