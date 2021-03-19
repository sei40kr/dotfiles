{ self, lib, ... }:

let
  inherit (builtins) isNull isFloat;
  inherit (lib)
    concatMapStringsSep escape isBool isInt isList isString replaceStrings;
  escapeEmacsLispString = v:
    escape [ "\\" ''"'' ]
    (replaceStrings [ "\n" "\r" "	" ] [ "\\n" "\\r" "\\t" ] v);
in rec {
  toEmacsLisp = v:
    if isNull v then
      "nil"
    else if isFloat v then
      toString v
    else if isInt v then
      toString v
    else if isBool v then
      (if v then "t" else "nil")
    else if isString v then
      ''"${escapeEmacsLispString v}"''
    else if isList v then
      "'(${concatMapStringsSep " " toEmacsLisp v})"
    else
      abort "toEmacsLisp: unexpected type (v = ${v})";
}
