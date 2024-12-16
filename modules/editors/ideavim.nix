{
  config,
  inputs,
  lib,
  ...
}:

with lib;
with lib.my;
let
  inherit (inputs) idea-LazyVim;
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

        source ${idea-LazyVim}/init.vim
      ''}

      " Use system clipboard
      set clipboard+=unnamed

      " Escape sequence
      imap jk <Esc>

      " Expand/contract region
      map <Leader>v <Action>(EditorSelectWord)
      let g:WhichKeyDesc_expand_region = '<leader>v Expand region'
      vmap v <Action>(EditorSelectWord)
      vmap V <Action>(EditorUnSelectWord)

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
