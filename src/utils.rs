use std::{
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    time::Instant,
};

use zip::ZipWriter;
use zip_extensions::ZipWriterExtensions;

pub fn time<R>(message: &str, runner: impl FnOnce() -> R) -> R {
    print!("{message} ");
    let start = Instant::now();
    let res = runner();
    let end = Instant::now();
    let duration = end - start;
    println!("took {:.5} ms", duration.as_micros() as f64 / 1000.);
    res
}

pub fn time_total<R>(message: &str, runner: impl FnOnce() -> R) -> R {
    let start = Instant::now();
    let res = runner();
    let end = Instant::now();
    let duration = end - start;
    println!("{} took {:.5} ms", message, duration.as_micros() as f64 / 1000.);
    res
}

pub fn write_json(json: &str, in_path: &Path, out_path: &Path, res_path: &Path) {
    let in_filename = in_path.file_name().and_then(|name| name.to_str());
    let out_name = in_filename.map(|name| name.split('.')).and_then(|mut parts| parts.next());
    let out_name = out_name.expect("input path should have a filename prefix");

    let out_folder = Path::new(&out_path).join(out_name);
    write_files(json, out_name, &out_folder, out_path, res_path);

    let _ = fs::remove_dir_all(&out_folder);
}

// needs to be a separate function to work on linux for some reason
// (probably to do with dropping file handles before attempting deletion)
fn write_files(json: &str, out_name: &str, out_folder: &PathBuf, out_path: &Path, res_path: &Path) {
    let _ = fs::DirBuilder::new().create(out_folder);
    let out_path_metadata = fs::metadata(out_folder).expect("out path should exist");
    assert!(out_path_metadata.is_dir(), "out path should be a dir");

    let mut project_file = File::create(out_folder.join("project.json"))
        .expect("should be able to create project file");

    project_file.write_all(json.as_bytes()).expect("should be able to write to out file");

    fs_extra::dir::copy(
        res_path,
        out_folder,
        &fs_extra::dir::CopyOptions::new().content_only(true),
    )
    .expect("res copy should succeed");

    let out_zip_path = Path::new(&out_path).join(format!("{out_name}.sb3"));
    let out_zip_file = File::create(out_zip_path).expect("should be able to create out zip file");
    let zip_writer = ZipWriter::new(out_zip_file);
    zip_writer.create_from_directory(out_folder).expect("creating zip should succeed");
}
