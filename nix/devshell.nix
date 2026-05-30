{
  perSystem =
    {
      pkgs,
      inputs',
      self',
      ...
    }:
    {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          self'.packages.kola-ls
          self'.packages.toolchain
          clippy
          rustfmt
          pkg-config
          lldb
          cargo-show-asm
          cargo-expand
          cargo-flamegraph
          wasm-tools
          python3
          # docs
          pnpm
          nodejs
        ];

        # Specify the rust-src path (many editors rely on this)
        RUST_SRC_PATH = "${inputs'.fenix.packages.complete.rust-src}/lib/rustlib/src/rust/library";
      };
    };
}
