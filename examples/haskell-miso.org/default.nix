{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) runCommand;
  inherit (pkgs.haskell.packages) ghcjs ghc802;
  miso-ghc = ghc802.callPackage ./../../miso-ghc.nix { };
  miso-ghcjs = ghcjs.callPackage ./../../miso-ghcjs.nix { };
  client = ghcjs.callPackage ./client { miso = miso-ghcjs; };
  server = ghc802.callPackage ./server { miso = miso-ghc; };
in
  runCommand "haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,js}
    cp ${client}/bin/client.jsexe/* $out/js
    cp ${server}/bin/* $out/bin
  ''

