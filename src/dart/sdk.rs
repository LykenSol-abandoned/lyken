use std::collections::HashMap;
use std::io::prelude::*;
use std::fs::File;
use std::path::{Path, PathBuf};

pub const PATH: &str = "flutter/bin/cache/dart-sdk";

thread_local!(pub static PLATFORM: Platform = Platform::load());

pub struct Platform {
    pub libraries: HashMap<String, PathBuf>,
}

impl Platform {
    fn load() -> Platform {
        let lib_path = Path::new(PATH).join("lib");
        let mut f = File::open(lib_path.join("dart_shared.platform")).unwrap();
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
                let mut libs = line.split(": ");
                let key = libs.next().unwrap().to_string();
                if let Ok(path) = lib_path.join(libs.next().unwrap()).canonicalize() {
                    platform.libraries.insert(key, path);
                }
            }
        }
        platform
    }
}
