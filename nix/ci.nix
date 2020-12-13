let
  haskell = import ./haskell.nix {};
in
{
  inherit (haskell.dtb-parse.components) all;
}
