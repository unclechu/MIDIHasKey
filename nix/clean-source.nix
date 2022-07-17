{ nix-gitignore }:

let
  # +1 and -1 for the slash after the prefix
  getRelativeFileName = prefix: fileName:
    builtins.substring
      (builtins.stringLength prefix + 1)
      (builtins.stringLength fileName - builtins.stringLength prefix - 1)
      fileName;

  srcFilter = prefix: fileName: fileType:
    let relativeFileName = getRelativeFileName prefix fileName; in
    (
      (fileType == "regular") &&
      (builtins.elem relativeFileName ["LICENSE" "Makefile"])
    )
    ||
    (
      (fileType == "regular") &&
      (builtins.match "^.+[.]cabal$" relativeFileName != null)
    )
    ||
    (
      (fileType == "directory") &&
      (builtins.match "^(app|src)(/.+)?$" relativeFileName != null)
    )
    ||
    (
      (fileType == "regular") &&
      (builtins.match "^(app|src)/.+[.](hs|hsc|c[+][+])$" relativeFileName != null)
    );

  cleanSrc = path:
    assert builtins.isPath path;
    nix-gitignore.gitignoreFilterRecursiveSource
      (srcFilter (toString path))
      [ ../.gitignore ]
      path;
in
  cleanSrc
