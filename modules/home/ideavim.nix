{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    optionalString
    types
    ;
  inherit (types) bool;

  editorsCfg = config.modules.editors;
  cfg = editorsCfg.ideavim;

  toVimScript =
    value:
    if builtins.isString value then
      "\"${value}\""
    else if builtins.isFloat value then
      toString value
    else if builtins.isInt value then
      toString value
    else if builtins.isBool value then
      if value then "1" else "0"
    else
      abort "toVimScript: unsupported type";
in
{
  imports = [
    inputs.self.homeModules.editor-shared
    inputs.self.homeModules.emacs
  ];

  options.modules.editors.ideavim = {
    enable = mkEnableOption "IdeaVim";

    doom.enable = mkOption {
      type = bool;
      default = editorsCfg.emacs.doom.enable;
      description = "Enable Doom Emacs-like keybindings";
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile."ideavim/ideavimrc".text = ''
      ${optionalString cfg.doom.enable ''
        let g:WhichKey_FontFamily = ${toVimScript editorsCfg.fonts.code.name}
        let g:WhichKey_FontSize = ${toVimScript editorsCfg.fonts.code.size}

        source ${inputs.idea-LazyVim}/init.vim
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
