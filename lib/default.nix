{ inputs, ... }:

let
  inherit (inputs.nixpkgs.lib)
    filterAttrs
    mapAttrs'
    mdDoc
    mkOption
    types
    ;
in
{
  # mapFilterAttrs ::
  #   (name -> value -> bool)
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs =
    pred: f: attrs:
    filterAttrs pred (mapAttrs' f attrs);

  extraTypes = {
    fontType = types.submodule {
      options = {
        package = mkOption {
          type = types.nullOr types.package;
          default = null;
          description = mdDoc ''
            The font package to use.
          '';
        };

        name = mkOption {
          type = types.str;
          example = "sans-serif";
          description = mdDoc ''
            The font name to use.
          '';
        };

        size = mkOption {
          type = types.int;
          example = 10;
          description = mdDoc ''
            The font size in points.
          '';
        };
      };
    };
  };
}
