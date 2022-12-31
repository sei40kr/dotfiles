_self: super:
let
  inherit (super) emacs fetchFromGitHub lib stdenv;
  inherit (emacs) version;
  inherit (lib) optionals;
  inherit (lib.versions) major;
  inherit (stdenv) isDarwin;

  emacs-plus = fetchFromGitHub {
    repo = "homebrew-emacs-plus";
    owner = "d12frosted";
    rev = "328a0beee56a4f099f9a6eb31290223238dd24bc";
    sha256 = "06vl4f6ihlgzxmzrfdndc5jg02qpycj9fivv218gkw2vbzfsnfv3";
  };
in
{
  emacs = (if major version != "28" then
    throw "Unsupported Emacs version: ${version}"
  else
    emacs.overrideAttrs ({ patches ? [ ], ... }: {
      patches = patches ++ (optionals isDarwin [
        "${emacs-plus}/patches/emacs-28/fix-window-role.patch"
        "${emacs-plus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
        "${emacs-plus}/patches/emacs-28/system-appearance.patch"
      ]);
    }));
}
