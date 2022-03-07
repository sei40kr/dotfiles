_self: super:
let
  inherit (super) emacsPgtk fetchFromGitHub lib stdenv;
  inherit (lib) optionals;
  inherit (stdenv) isDarwin;
  emacs-plus = fetchFromGitHub {
    repo = "homebrew-emacs-plus";
    owner = "d12frosted";
    rev = "b7809dd815e7753e20851c81603c82a573d7d1cc";
    sha256 = "0l0hjp1mqgj3l50adgrm2kin4d6h5mzhf0jg3xmwrwryg9ywlwqk";
  };
in {
  emacsPgtk = emacsPgtk.overrideAttrs ({ patches ? [ ], ... }: {
    patches = patches ++ (optionals isDarwin [
      "${emacs-plus}/patches/emacs-29/fix-window-role.patch"
      "${emacs-plus}/patches/emacs-29/no-frame-refocus-cocoa.patch"
      "${emacs-plus}/patches/emacs-29/system-appearance.patch"
    ]);
  });
}
