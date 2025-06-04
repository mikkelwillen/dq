{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  shellHook = ''
    fish
  '';
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
