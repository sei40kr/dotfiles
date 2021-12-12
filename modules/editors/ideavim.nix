{ config, inputs, lib, ... }:

with lib;
with lib.my;
let
  inherit (inputs) idea-doom-emacs;
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.ideavim;
in {
  options.modules.editors.ideavim = {
    enable = mkBoolOpt false;

    doom.enable = mkOption {
      type = types.bool;
      default = editorsCfg.emacs.doom.enable;
    };
  };

  config = mkIf cfg.enable {
    home.configFile."ideavim/ideavimrc".text = ''
      ${optionalString cfg.doom.enable ''
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
