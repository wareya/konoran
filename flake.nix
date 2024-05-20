{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        mkKonoran =
          {
            lib,
            musl,
            zlib,
            ncurses,
            libxml2,
            libffi,
            llvmPackages_18,
            rustPlatform,
            static ? false,
            ...
          }:
          let
            fakeCxx = pkgs.runCommand "fakecxx" { } ''
              install -D ${pkgs.pkgsStatic.stdenv.cc.cc.lib}/lib/libstdc++.a $out/lib/libc++.a
            '';
          in
          rustPlatform.buildRustPackage {
            pname = "konoran";
            version = "unstable";
            src = self;
            cargoLock = {
              lockFile = ./Cargo.lock;
              allowBuiltinFetchGit = true;
            };
            nativeBuildInputs = [ llvmPackages_18.llvm ];
            meta.mainProgram = "konoran";
            RUSTFLAGS =
              [
                "-L ${libffi.out}/lib"
                "-L ${zlib.out}/lib"
                "-L ${ncurses.out}/lib"
                "-L ${libxml2.out}/lib"
              ]
              ++ lib.optionals static [
                "-L ${musl.out}/lib"
                "-L ${fakeCxx.out}/lib"
                "-C relocation-model=static"
                "-C target-feature=+crt-static"
                "-C link-args=-Wl,--no-as-needed"
                "-C link-args=-static"
                "-C link-args=${musl.out}/lib/libc.a"
              ];
          };
        # TODO: Not sure why you can't just use pkgs.pkgsStatic.callPackage
        konoranStatic = mkKonoran ({ static = true; } // pkgs.pkgsStatic);
        konoran = pkgs.callPackage mkKonoran { };
      in
      {
        packages = {
          inherit konoran konoranStatic;
          default = konoran;
        };
        devShell = pkgs.mkShell { inputsFrom = [ konoran ]; };
      }
    );
}
