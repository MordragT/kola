use std::fs;

use zed_extension_api as zed;

pub struct KolaExtension {}

impl zed::Extension for KolaExtension {
    fn new() -> Self
    where
        Self: Sized,
    {
        KolaExtension {}
    }

    fn language_server_command(
        &mut self,
        id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let env = worktree.shell_env();

        let command = if let Some(path) = worktree.which("kola-ls") {
            path
        } else if let Some(path) = env
            .iter()
            .find_map(|(key, value)| (key == "KOLA_LANGUAGE_SERVER").then_some(value))
        {
            path.to_owned()
        } else {
            zed::set_language_server_installation_status(
                id,
                &zed_extension_api::LanguageServerInstallationStatus::CheckingForUpdate,
            );

            let release = zed::latest_github_release(
                "MordragT/kola",
                zed::GithubReleaseOptions {
                    pre_release: false,
                    require_assets: true,
                },
            )?;
            let (os, arch) = zed::current_platform();

            let asset_name = format!(
                "kola-ls-{os}-{arch}",
                os = match os {
                    zed::Os::Mac => "darwin",
                    zed::Os::Linux => "linux",
                    zed::Os::Windows => "win32",
                },
                arch = match arch {
                    zed::Architecture::Aarch64 => "arm64",
                    zed::Architecture::X86 => "x86",
                    zed::Architecture::X8664 => "x64",
                },
            );

            let asset = release
                .assets
                .iter()
                .find(|asset| asset.name == asset_name)
                .ok_or("Kola Language Server not found".to_owned())?;

            let release_dir = format!("kola-ls-{}", release.version);

            let binary_path = format!("{release_dir}/kola-ls");

            if !fs::metadata(&binary_path).map_or(false, |stat| stat.is_file()) {
                zed::set_language_server_installation_status(
                    id,
                    &zed::LanguageServerInstallationStatus::Downloading,
                );

                zed::download_file(
                    &asset.download_url,
                    &binary_path,
                    zed::DownloadedFileType::Uncompressed,
                )?;

                zed::make_file_executable(&binary_path)?;
            }

            binary_path
        };

        let settings = zed::settings::LspSettings::for_worktree("kola-ls", worktree)?;

        let args = settings
            .binary
            .as_ref()
            .and_then(|binary| binary.arguments.clone())
            .unwrap_or_default();

        Ok(zed::Command { command, args, env })
    }
}

zed::register_extension!(KolaExtension);
