{ config, inputs, lib, ... }:

with lib;
with lib.my;
let
  inherit (inputs) idea-doom-emacs;
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.ideavim;
in
{
  options.modules.editors.ideavim = {
    enable = mkBoolOpt false;

    doom.enable = mkBoolOpt editorsCfg.emacs.doom.enable;
  };

  config = mkIf cfg.enable {
    home.configFile."ideavim/ideavimrc".text = ''
      ${optionalString cfg.doom.enable ''
        let g:WhichKey_FontFamily = ${toVimScript editorsCfg.fonts.code.name}
        let g:WhichKey_FontSize = ${toVimScript editorsCfg.fonts.code.size}

        source ${idea-doom-emacs}/init.vim
        source ${idea-doom-emacs}/expand-region.vim
      ''}

      " Alt-Left/Right to switch tabs
      nnoremap <A-Left>  :tabp<CR>
      nnoremap <A-Right> :tabn<CR>

      " Alt-Shift-Left/Right to move tabs
      nnoremap <A-S-Left>  :tabm -1<CR>
      nnoremap <A-S-Right> :tabm +1<CR>

      " Do not exit visual mode on a selection shift
      vnoremap < <gv
      vnoremap > >gv
    '';
  };
}
