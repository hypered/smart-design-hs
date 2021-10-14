let

  sources = import ./sources.nix;
  defNixpkgs = import sources.nixpkgs { };

in { nixpkgs ? defNixpkgs }:

let inherit (nixpkgs.lib.attrsets) getAttrFromPath;
in {
  # Lists all packages made available through this nix project.
  # The format is `{ <pkgName> : <pkgDir> }` (we refer to this as pInfo)
  # The used directory should be the path of the directory relative to the root of the project.
  pkgList = { design-hs-lib = ../lib; };
  # Get an attribute from a string path from a larger attrSet
  getPkg = pkgs: pPath: getAttrFromPath [pPath] pkgs;
}

