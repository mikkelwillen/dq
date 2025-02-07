{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  name = "dq";
  buildInputs =
    [pkgs.cacert
     pkgs.curl
     pkgs.git
     pkgs.unzip
     pkgs.mlkit
     pkgs.futhark
     pkgs.mlton
     pkgs.smlpkg
    ];
}
