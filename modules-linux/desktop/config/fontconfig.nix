{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.config.fontconfig;
  fcBool = v: "<bool>" + (if v then "true" else "false") + "</bool>";
in {
  options.modules.desktop.config.fontconfig = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    antialias = mkOption {
      type = types.bool;
      default = true;
      description = "Enable font antialiasing.";
    };

    defaultFonts = {
      monospace = mkOption {
        type = with types; listOf str;
        default = [ "Noto Sans Mono" ];
        description = ''
          System-wide default monospace font(s). Multiple fonts may be
          listed in case multiple languages must be supported.
        '';
      };

      sansSerif = mkOption {
        type = with types; listOf str;
        default = [ "Noto Sans" ];
        description = ''
          System-wide default sans serif font(s). Multiple fonts may be
          listed in case multiple languages must be supported.
        '';
      };

      serif = mkOption {
        type = with types; listOf str;
        default = [ "Noto Serif" ];
        description = ''
          System-wide default serif font(s). Multiple fonts may be listed
          in case multiple languages must be supported.
        '';
      };
    };

    dpi = mkOption {
      type = types.int;
      default = 0;
      description = ''
        Force DPI setting. Setting to <literal>0</literal> disables DPI
        forcing; the DPI detected for the display will be used.
      '';
    };

    hinting = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable TrueType hinting.";
      };

      autohint = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable the autohinter, which provides hinting for otherwise
          un-hinted fonts. The results are usually lower quality than
          correctly-hinted fonts.
        '';
      };

      style = mkOption {
        type = types.enum [ "none" "slight" "medium" "full" ];
        default = "full";
        description = ''
          TrueType hinting style, one of <literal>none</literal>,
          <literal>slight</literal>, <literal>medium</literal>, or
          <literal>full</literal>.
        '';
      };
    };

    subpixel = {
      lcdfilter = mkOption {
        default = "default";
        type = types.enum [ "none" "default" "light" "legacy" ];
        description = ''
          FreeType LCD filter, one of <literal>none</literal>,
          <literal>default</literal>, <literal>light</literal>, or
          <literal>legacy</literal>.
        '';
      };

      rgba = mkOption {
        default = "rgb";
        type = types.enum [ "rgb" "bgr" "vrgb" "vbgr" "none" ];
        description = ''
          Subpixel order, one of <literal>none</literal>,
          <literal>rgb</literal>, <literal>bgr</literal>,
          <literal>vrgb</literal>, or <literal>vbgr</literal>.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    my.home.fonts.fontconfig.enable = mkForce true;

    my.home.xdg.configFile = {
      "fontconfig/conf.d/10-nix-rendering.conf".text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <match target="font">
            <edit mode="assign" name="antialias">
              ${fcBool cfg.antialias}
            </edit>
            <edit mode="assign" name="hinting">
              ${fcBool cfg.hinting.enable}
            </edit>
            <edit mode="assign" name="autohint">
              ${fcBool cfg.hinting.autohint}
            </edit>
            <edit mode="assign" name="hintstyle">
              <const>hint${cfg.hinting.style}</const>
            </edit>
            <edit mode="assign" name="rgba">
              <const>${cfg.subpixel.rgba}</const>
            </edit>
            <edit mode="assign" name="lcdfilter">
              <const>lcd${cfg.subpixel.lcdfilter}</const>
            </edit>
          </match>
          ${
            optionalString (cfg.dpi != 0) ''
              <match target="pattern">
                <edit name="dpi" mode="assign">
                  <double>${toString cfg.dpi}</double>
                </edit>
              </match>
            ''
          }
        </fontconfig>
      '';
      "fontconfig/conf.d/60-generic-aliases.conf".text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          ${
            optionalString (cfg.defaultFonts.sansSerif != [ ]) ''
              <alias binding="same">
                <family>sans-serif</family>
                <prefer>
                  ${
                    concatStringsSep "\n"
                    (map (font: "<family>${font}</family>")
                      cfg.defaultFonts.sansSerif)
                  }
                </prefer>
              </alias>
            ''
          }
          ${
            optionalString (cfg.defaultFonts.serif != [ ]) ''
              <alias binding="same">
                <family>serif</family>
                <prefer>
                  ${
                    concatStringsSep "\n"
                    (map (font: "<family>${font}</family>")
                      cfg.defaultFonts.serif)
                  }
                </prefer>
              </alias>
            ''
          }
          ${
            optionalString (cfg.defaultFonts.monospace != [ ]) ''
              <alias binding="same">
                <family>monospace</family>
                <prefer>
                  ${
                    concatStringsSep "\n"
                    (map (font: "<family>${font}</family>")
                      cfg.defaultFonts.monospace)
                  }
                </prefer>
              </alias>
            ''
          }
        </fontconfig>
      '';
    };
  };
}
