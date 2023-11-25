{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ocaml;
in
{
  options.modules.dev.ocaml = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable OCaml development environment.
      '';
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ocaml
      ocamlPackages.findlib
      dune_3
      ocamlformat
      ocamlPackages.utop
      ocamlPackages.ocaml-lsp
    ];

    # TODO: Extract these colors from a Neovim theme
    home.configFile."utoprc".text = ''
      profile:                dark
      identifier.foreground:  none
      module.foreground:      cyan
      comment.foreground:     x-chocolate1
      doc.foreground:         x-light-salmon
      constant.foreground:    yellow
      keyword.foreground:     magenta
      symbol.foreground:      magenta
      string.foreground:      green
      char.foreground:        green
      quotation.foreground:   cyan
      error.foreground:       red
      directive.foreground:   x-lightsteelblue
      parenthesis.foreground: none
    '';

    home.configFile."utop/init.ml".text = ''
      #utop_prompt_dummy;;
      UTop.set_show_box false
    '';
  };
}
