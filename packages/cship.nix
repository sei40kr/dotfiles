{ pkgs }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "cship";
  version = "1.8.3";

  src = pkgs.fetchCrate {
    inherit pname version;
    hash = "sha256-1ZKsnf4MdPjjCYDtempmm+cl9IiUj/A1f5Hvy/FLrnA=";
  };

  cargoLock.lockFile = "${src}/Cargo.lock";

  meta = with pkgs.lib; {
    description = "Starship-style statusline for Claude Code";
    homepage = "https://github.com/stephenleo/cship";
    license = licenses.asl20;
    mainProgram = "cship";
    platforms = platforms.unix;
  };
}
