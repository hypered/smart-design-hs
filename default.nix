let haskellPkgs = import ./design-hs-haskell.nix;
in haskellPkgs // {
  design-hs-html = (import ./design-hs-html.nix {}).design-hs-html;
  static = (import ./design-hs-html.nix {}).static;
}
