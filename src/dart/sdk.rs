use git2::{Oid, Repository};
use std::collections::HashMap;
use std::env;
use std::io::prelude::*;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::process::{self, Command};
use url::Url;

const FLUTTER_REPO: &str = "https://github.com/lykenware/flutter";
const FLUTTER_REPO_REV: &str = "e16e4024164756de13d7ca1b3ffad385229b840b";

thread_local!(static FLUTTER_PATH: PathBuf = {
    #[cfg(windows)]
    let cache_dir = PathBuf::from(env::var_os("APPDATA").unwrap()).join("Lyken/cache");

    #[cfg(unix)]
    let cache_dir = xdg::BaseDirectories::with_prefix("lyken").unwrap()
        .create_cache_directory("").unwrap();

    fs::create_dir_all(&cache_dir).unwrap();
    let flutter_dir = cache_dir.join("flutter");
    if !flutter_dir.exists() {
        let flutter_tmp_dir = cache_dir.join("flutter-tmp");
        if flutter_tmp_dir.is_dir() {
            fs::remove_dir_all(&flutter_tmp_dir).unwrap();
        }
        let repo = Repository::clone(FLUTTER_REPO, &flutter_tmp_dir).unwrap();
        repo.set_head_detached(Oid::from_str(FLUTTER_REPO_REV).unwrap()).unwrap();
        repo.checkout_head(None).unwrap();
        drop(repo);
        fs::rename(&flutter_tmp_dir, &flutter_dir).unwrap();
    }
    flutter_dir
});

const ENGINE_SUBPATH: &str = "bin/cache/pkg/sky_engine";
const SDK_SUBPATH: &str = "bin/cache/dart-sdk";

pub fn with_cmd<F: FnOnce(&mut Command) -> R, R>(f: F) -> R {
    let (sh, dash_c) = if cfg!(target_os = "windows") {
        ("cmd", "/C")
    } else {
        ("sh", "-c")
    };
    FLUTTER_PATH.with(|flutter_path| {
        let path_env_var = env::join_paths(
            env::var_os("PATH")
                .iter()
                .flat_map(env::split_paths)
                .chain(Some(flutter_path.join(SDK_SUBPATH).join("bin")))
                .chain(Some(flutter_path.join("bin"))),
        ).unwrap();
        f(
            Command::new(sh)
                .arg(dash_c)
                .env("PATH", path_env_var)
                .env("FLUTTER_ROOT", flutter_path),
        )
    })
}

thread_local!(pub static PLATFORM: Platform = Platform::load());

pub struct Platform {
    pub libraries: HashMap<String, PathBuf>,
}

impl Platform {
    fn load() -> Platform {
        let lib_path = FLUTTER_PATH.with(|p| p.join(SDK_SUBPATH).join("lib"));
        if !lib_path.exists() {
            with_cmd(|cmd| {
                let status = cmd.arg("flutter precache").status().unwrap();
                if !status.success() {
                    process::exit(1);
                }
            });
        }
        let mut f = File::open(lib_path.join("dart_server.platform")).unwrap();
        let mut text = String::new();
        f.read_to_string(&mut text).unwrap();
        let mut platform = Platform {
            libraries: HashMap::new(),
        };
        let lines = text.lines();
        let mut in_libraries = false;
        for line in lines {
            if line == "[libraries]" {
                in_libraries = true;
                continue;
            }
            if in_libraries {
                let mut lib = line.splitn(2, ": ");
                let key = lib.next().unwrap().to_string();
                if let Ok(path) = lib_path.join(lib.next().unwrap()).canonicalize() {
                    platform.libraries.insert(key, path);
                }
            }
        }
        platform.libraries.insert(
            String::from("ui"),
            FLUTTER_PATH.with(|flutter_path| {
                flutter_path
                    .join(ENGINE_SUBPATH)
                    .join("lib/ui/ui.dart")
                    .canonicalize()
                    .unwrap()
            }),
        );
        platform
    }
}

thread_local!(pub static FLUTTER_PACKAGES: Packages = Packages::load(Path::new(".")));

pub struct Packages {
    pub packages: HashMap<String, PathBuf>,
}

impl Packages {
    fn load(base_path: &Path) -> Packages {
        PLATFORM.with(|_| {});
        let path = base_path.join(".packages");
        if !path.exists() {
            with_cmd(|cmd| {
                let status = cmd.arg("pub get").current_dir(base_path).status().unwrap();
                if !status.success() {
                    process::exit(1);
                }
            });
        }
        let mut f = File::open(path).unwrap();
        let mut text = String::new();
        f.read_to_string(&mut text).unwrap();
        let mut packages = Packages {
            packages: HashMap::new(),
        };
        let lines = text.lines();
        let base_path = base_path.canonicalize().unwrap();
        let base_url = Url::from_directory_path(base_path).unwrap();
        for line in lines {
            if line.starts_with("#") {
                continue;
            }
            let mut pkg = line.splitn(2, ':');
            let key = pkg.next().unwrap().to_string();
            let url = base_url.join(pkg.next().unwrap()).unwrap();
            if let Ok(path) = url.to_file_path().unwrap().canonicalize() {
                packages.packages.insert(key, path);
            }
        }
        packages
    }
}

pub fn resolve_import(uri: &str) -> PathBuf {
    let mut uri_parts = uri.split('/');
    let mut path = if uri.starts_with("dart:") {
        let prefix = uri_parts.next().unwrap();
        PLATFORM.with(|p| {
            p.libraries
                .get(&prefix["dart:".len()..])
                .cloned()
                .unwrap_or_else(|| PathBuf::from(prefix))
        })
    } else if uri.starts_with("package:") {
        let prefix = uri_parts.next().unwrap();
        FLUTTER_PACKAGES.with(|p| {
            p.packages
                .get(&prefix["package:".len()..])
                .cloned()
                .unwrap_or_else(|| PathBuf::from(prefix))
        })
    } else {
        PathBuf::new()
    };
    path.extend(uri_parts);
    path
}
