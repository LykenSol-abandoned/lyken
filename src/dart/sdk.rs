use std::collections::HashMap;
use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{self, Command};
use url::Url;

pub const FLUTTER_PATH: &str = "flutter/";
pub const ENGINE_PATH: &str = "flutter/bin/cache/pkg/sky_engine/lib/";
pub const PATH: &str = "flutter/bin/cache/dart-sdk/";

pub fn with_cmd<F: FnOnce(&mut Command) -> R, R>(f: F) -> R {
    let (sh, dash_c) = if cfg!(target_os = "windows") {
        ("cmd", "/C")
    } else {
        ("sh", "-c")
    };
    let lyken_dir = env::current_dir().unwrap();
    let path_env_var = env::join_paths(
        env::var_os("PATH")
            .iter()
            .flat_map(env::split_paths)
            .chain(Some(lyken_dir.join(&Path::new(PATH).join("bin"))))
            .chain(Some(lyken_dir.join(&Path::new(FLUTTER_PATH).join("bin")))),
    ).unwrap();
    f(
        Command::new(sh)
            .arg(dash_c)
            .env("PATH", path_env_var)
            .env("FLUTTER_ROOT", FLUTTER_PATH),
    )
}

thread_local!(pub static PLATFORM: Platform = Platform::load());

pub struct Platform {
    pub libraries: HashMap<String, PathBuf>,
}

impl Platform {
    fn load() -> Platform {
        let lib_path = Path::new(PATH).join("lib");
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
            Path::new(ENGINE_PATH)
                .join("ui/ui.dart")
                .canonicalize()
                .unwrap(),
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
