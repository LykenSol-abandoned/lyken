use std::collections::HashMap;
use std::io::prelude::*;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use dart::ast::Module;
use node::Node;
use url::Url;

pub const FLUTTER_PATH: &str = "flutter/";
pub const ENGINE_PATH: &str = "flutter/bin/cache/pkg/sky_engine/lib/";
pub const PATH: &str = "flutter/bin/cache/dart-sdk/";

thread_local!(pub static PLATFORM: Platform = Platform::load());

pub struct Platform {
    pub libraries: HashMap<String, PathBuf>,
}

impl Platform {
    fn load() -> Platform {
        let lib_path = Path::new(PATH).join("lib");
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
        let path = base_path.join(".packages");
        if !path.exists() {
            let (sh, dash_c) = if cfg!(target_os = "windows") {
                ("cmd", "/C")
            } else {
                ("sh", "-c")
            };
            let status = Command::new(sh)
                .arg(dash_c)
                .arg("pub get")
                .current_dir(base_path)
                .env("PATH", Path::new(PATH).join("bin"))
                .env("FLUTTER_ROOT", FLUTTER_PATH)
                .stdout(Stdio::null())
                .status()
                .unwrap();
            if !status.success() {
                process::exit(1);
            }
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

pub fn resolve_import(root_module: Node<Module>, uri: &str) -> Node<Module> {
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
        root_module.path.parent().unwrap().to_path_buf()
    };
    path.extend(uri_parts);
    Module::load(&path)
}
