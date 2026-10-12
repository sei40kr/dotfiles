{ pkgs }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "jj-hunk-tool";
  version = "0.1.0-unstable-2026-10-10";

  src = pkgs.fetchFromGitHub {
    owner = "mvzink";
    repo = "jj-hunk-tool";
    rev = "817a3d19cab8ed9bf04ebf64f2f3073fe195d641";
    hash = "sha256-HtuR2IL/WIq6+y8saHFAyquQ8cFGvvz0t3VqZXBQ7gI=";
  };

  cargoLock.lockFile = "${src}/Cargo.lock";

  nativeCheckInputs = [ pkgs.jujutsu ];

  meta = with pkgs.lib; {
    description = "Non-interactive hunk level jj operations";
    homepage = "https://github.com/mvzink/jj-hunk-tool";
    license = licenses.mit;
    mainProgram = "jj-hunk-tool";
    platforms = platforms.unix;
  };
}
