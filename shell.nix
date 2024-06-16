{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell rec {
    buildInputs = with pkgs;
    [
      cabal-install
      haskell.compiler.ghc982
      pkg-config
      freeglut
      glfw
      libGLU
      libGL
      xorg.libX11
      xorg.libXi
      xorg.libXrandr
      xorg.libXxf86vm
      xorg.libXcursor
      xorg.libXinerama
      xorg.libXdmcp
      xorg.libXext
      xorg.libXfixes
      zlib.dev
      (pkgs.haskell-language-server.override { supportedGhcVersions = [ "982" ]; })
    ];
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
}
