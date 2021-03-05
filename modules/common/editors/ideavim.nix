{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.ideavim;
in {
  options.modules.editors.ideavim = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home.file.".ideavimrc".text = ''
      " Do not exit visual mode on a selection shift
      vnoremap < <gv
      vnoremap > >gv
    '';
  };
}
