{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell rec {
  buildInputs = with pkgs; [ clang m4 binutils pkgconfig gmp autoconf libseccomp inotify-tools glibc ];

  shellHook = ''
    eval $(opam env)
  '';
}
