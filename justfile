#!/usr/bin/env -S nix shell nixpkgs#nushell nixpkgs#just --command 'just --justfile'

nvim:
    nvim -u .nvim/init.lua

install-zed-ext:
    #!/usr/bin/env nu
    const ext_dir = [ $nu.home-path .local share zed extensions installed kola-zed-ext] | path join

    cargo build -p kola-zed-ext --target=wasm32-wasip1 --release

    let temp_dir = mktemp -d

    print $temp_dir

    let adaptor = $temp_dir | path join wasi_snapshot_preview1.wasm
    let ext = $temp_dir | path join extension.wasm

    http get "https://github.com/bytecodealliance/wasmtime/releases/download/v31.0.0/wasi_snapshot_preview1.reactor.wasm" | save $adaptor

    (wasm-tools component new target/wasm32-wasip1/release/kola_zed_ext.wasm
        --adapt $"wasi_snapshot_preview1=($adaptor)"
        --output $ext
    )

    wasm-tools validate $ext

    print "Copying built extension"

    if ($ext_dir | path exists) { rm -r $ext_dir }

    mkdir $ext_dir

    cp -r crates/kola-zed-ext/languages ($ext_dir | path join languages)
    cp crates/kola-zed-ext/extension.toml ($ext_dir | path join extension.toml)
    cp $ext ($ext_dir | path join extension.wasm)

    rm -r $temp_dir
