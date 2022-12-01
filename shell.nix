{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      foldl
      profunctors
      split
      vector
    ]))
  ];
}
