{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.pkgsStatic;
        fakeCxx = pkgs.runCommand "fakecxx" {} ''
          install -D ${pkgs.stdenv.cc.cc.lib}/lib/libstdc++.a $out/lib/libc++.a
        '';
        konoran = pkgs.rustPlatform.buildRustPackage {
          pname = "konoran";
          version = "unstable";
          src = ./.;
          cargoLock = {
            lockFile = ./Cargo.lock;
            allowBuiltinFetchGit = true;
          };
          nativeBuildInputs = with pkgs; [
            llvmPackages_18.llvm
          ];
          RUSTFLAGS = [
            "-C relocation-model=static"
            "-C target-feature=+crt-static"
            "-L ${pkgs.musl.out}/lib"
            "-L ${pkgs.zlib.out}/lib"
            "-L ${pkgs.ncurses.out}/lib"
            "-L ${pkgs.libxml2.out}/lib"
            "-L ${pkgs.libffi.out}/lib"
            "-L ${fakeCxx.out}/lib"
            "-C link-args=-Wl,--no-as-needed"
            "-C link-args=-static"
            "-C link-args=${pkgs.musl.out}/lib/libc.a"
          ];

        };
       in {
        packages = {
          inherit konoran;
          default = konoran;
        };
        devShell = pkgs.mkShell {
          inputsFrom = [konoran];
        };
      }
    );
}
