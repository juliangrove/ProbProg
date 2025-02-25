let nixpkgs_source = (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-22.05.tar.gz);
in
{ pkgs ? import nixpkgs_source {
    inherit system;
  }
, system ? builtins.currentSystem
}:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; ([
    cabal-install
    gasp
  ]));
in
pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [
    ghc
  ];
  shellHook = ''
    export LANG=C.UTF-8
    export LC_ALL=C.UTF-8
    # export LANG=en_US.UTF-8
    # export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}
