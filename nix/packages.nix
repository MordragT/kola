{
  perSystem =
    { pkgs, inputs', ... }:
    {
      packages =
        let
          toolchain =
            with inputs'.fenix.packages;
            combine [
              latest.toolchain
              targets.wasm32-unknown-unknown.latest.rust-std
              targets.wasm32-wasip1.latest.rust-std
              targets.wasm32-wasip2.latest.rust-std
            ];

          platform = pkgs.makeRustPlatform {
            # Use nightly rustc and cargo provided by fenix for building
            cargo = toolchain;
            rustc = toolchain;
          };

          kola = platform.buildRustPackage {
            pname = "kola";
            version = "0.1.0";
            src = ../.;
            cargoLock.lockFile = ../Cargo.lock;
          };
        in
        {
          inherit kola toolchain;

          default = kola;

          kola-ls = platform.buildRustPackage {
            pname = "kola-ls";
            version = "0.1.0";
            src = ../.;
            cargoLock.lockFile = ../Cargo.lock;
            cargoBuildFlags = [
              "-p"
              "kola-ls"
            ];
          };
        };
    };
}
