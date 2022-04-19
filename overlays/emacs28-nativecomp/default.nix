_self: super:
let
  inherit (super) lib stdenv;
  emacs-plus = super.fetchFromGitHub {
    repo = "homebrew-emacs-plus";
    owner = "d12frosted";
    rev = "328a0beee56a4f099f9a6eb31290223238dd24bc";
    sha256 = "06vl4f6ihlgzxmzrfdndc5jg02qpycj9fivv218gkw2vbzfsnfv3";
  };
in
{
  emacs28NativeComp = super.emacs28NativeComp.overrideAttrs ({ patches ? [ ], ... }: {
    patches = patches ++ (lib.optionals stdenv.isDarwin [
      "${emacs-plus}/patches/emacs-28/fix-window-role.patch"
      "${emacs-plus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
      "${emacs-plus}/patches/emacs-28/system-appearance.patch"
    ]);
  });
}
