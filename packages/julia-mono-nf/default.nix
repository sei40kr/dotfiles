{
  lib,
  julia-mono,
  nerd-font-patcher,
  runCommand,
}:

runCommand "julia-mono-nf"
  {
    pname = "JuliaMono-nf";
    version = julia-mono.version;

    nativeBuildInputs = [
      julia-mono
      nerd-font-patcher
    ];

    meta = with lib; {
      description = "Composite font of JuliaMono and nerd-fonts";
      license = licenses.ofl;
      platforms = platforms.all;
    };
  }
  ''
    mkdir -p $out/share/fonts/truetype
    for font in ${julia-mono}/share/fonts/truetype/JuliaMono-*.ttf; do
      nerd-font-patcher -q \
                        -out $out/share/fonts/truetype \
                        -c \
                        --no-progressbars \
                        $font
    done
  ''
