{
  description = "Rust development template";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    fenix,
    ...
  }:
    utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [fenix.overlays.default];
        };
        toolchain = with pkgs.fenix;
          combine [
            latest.toolchain
            targets.wasm32-unknown-unknown.latest.rust-std
            targets.wasm32-wasip1.latest.rust-std
            targets.wasm32-wasip2.latest.rust-std
          ];

        platform = pkgs.makeRustPlatform {
          # Use nightly rustc and cargo provided by fenix for building
          inherit (toolchain) cargo rustc;
        };
      in rec
      {
        # Executed by `nix build`
        packages.default = self.packages."${system}".kola;

        packages.kola = platform.buildRustPackage {
          pname = "kola";
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };

        packages.kola-ls = platform.buildRustPackage {
          pname = "kola-ls";
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
          cargoBuildFlags = ["-p" "kola-ls"];
        };

        # Executed by `nix run`
        apps.default = utils.lib.mkApp {drv = packages.default;};

        # Used by `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            toolchain
            clippy
            rustfmt
            pkg-config
            lldb
            cargo-expand
            wasm-tools
          ];

          # Specify the rust-src path (many editors rely on this)
          RUST_SRC_PATH = "${pkgs.fenix.complete.rust-src}/lib/rustlib/src/rust/library";
        };
      }
    );
}
