use std::collections::HashMap;
use std::io::prelude::*;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use dart::ast::Module;
use node::Node;
use url::Url;

pub const FLUTTER_PATH: &str = "flutter/";
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
    if uri.starts_with("dart:") {
        let lib = &uri["dart:".len()..];
        PLATFORM.with(|p| Module::load(&p.libraries[lib]))
    } else if uri.starts_with("package:") {
        let mut uri_parts = uri["package:".len()..].splitn(2, '/');
        let name = uri_parts.next().unwrap();
        let path = uri_parts.next().unwrap();
        FLUTTER_PACKAGES.with(|p| Module::load(&p.packages[name].join(path)))
    } else {
        Module::load(&root_module.path.parent().unwrap().join(uri))
    }
}
