{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  ghcEnv = pkgs.haskellPackages.ghcWithPackages (p: with p;
    [
    base
    attoparsec
    ]);

in
with pkgs; stdenv.mkDerivation {
  name = "Cogs_Env";
  buildInputs = [ ghcEnv ];
}
