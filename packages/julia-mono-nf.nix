{ pkgs }:

pkgs.runCommand "julia-mono-nf"
  {
    inherit (pkgs.julia-mono) version;

    pname = "JuliaMono-nf";

    nativeBuildInputs = [
      pkgs.julia-mono
      pkgs.nerd-font-patcher
    ];

    meta = with pkgs.lib; {
      description = "Composite font of JuliaMono and nerd-fonts";
      license = licenses.ofl;
      platforms = platforms.all;
    };
  }
  ''
    mkdir -p $out/share/fonts/truetype
    for font in ${pkgs.julia-mono}/share/fonts/truetype/JuliaMono-*.ttf; do
      nerd-font-patcher -q \
                        -out $out/share/fonts/truetype \
                        -c \
                        --no-progressbars \
                        $font
    done
  ''
