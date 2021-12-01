{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.ideavim;

  idea-doom-emacs = ../../repos/idea-doom-emacs;
in {
  options.modules.editors.ideavim = {
    enable = mkBoolOpt false;
    enableDoom = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.configFile."ideavim/ideavimrc".text = ''
      ${optionalString cfg.enableDoom ''
        let g:WhichKey_FontFamily = ${toVimScript editorsCfg.fonts.code.family}
        let g:WhichKey_FontSize = ${toVimScript editorsCfg.fonts.code.size}

        source ${idea-doom-emacs}/ideavimrc
        source ${idea-doom-emacs}/expand-region.vim
      ''}

      " Do not exit visual mode on a selection shift
      vnoremap < <gv
      vnoremap > >gv
    '';
  };
}
