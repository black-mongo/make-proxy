use rustler::{NifTaggedEnum};
use inn_common::genca::CertAuthority;
use rustler::ResourceArc;
use rustler::types::atom::ok;
use rustler::Term;
use rustler::NifResult;
use rustler::Encoder;
use rustler::Env;
use std::sync::RwLock;
use std::sync::RwLockWriteGuard;
use std::sync::RwLockReadGuard;
// See Derive Macros docs at https://docs.rs/rustler/0.26.0/rustler/index.html#derives
// #[derive(NifTuple)]
// struct CaInfo{
//    common_name: String,
//    org: String,
//    country_name: String,
//    locality_name: String ,
//    output: String
// }
#[derive(NifTaggedEnum)]
enum CaInfo{
   CaInfo(String, String, String, String, String)
}
#[derive(NifTaggedEnum)]
enum Res{
    Ok,
    Error(String)
}

#[repr(transparent)]
struct NifCaResource(RwLock<CertAuthority>);

impl NifCaResource {
    fn write(&self) -> RwLockWriteGuard<'_, CertAuthority> {
    self.0.write().unwrap()
    }
    fn read(&self) -> RwLockReadGuard<'_, CertAuthority> {
    self.0.read().unwrap()
    }
}

impl From<CertAuthority> for NifCaResource{
    fn from(other: CertAuthority) -> Self {
        NifCaResource(RwLock::new(other))
    }
}

pub fn on_load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(NifCaResource, env);
    true
}

// =================================================================================================
// api
// =================================================================================================

#[rustler::nif]
fn get_cainfo() -> CaInfo{
    CaInfo::CaInfo("".into(),"".into(),"".into(),"".into(),"".into())
}
#[rustler::nif]
fn new(env: Env, cacert_file: String, cakey_file: String) -> NifResult<Term> {
    let rs = CertAuthority::new(cacert_file, cakey_file);
    Ok((ok(), ResourceArc::new(NifCaResource::from(rs))).encode(env))
}
#[rustler::nif]
fn gen_ca(info: CaInfo) -> Res {
    match info{
        CaInfo::CaInfo(common_name, org, country_name, locality_name, output) =>{
        match CertAuthority::gen_ca(common_name, org, country_name, locality_name, output){
        Ok(_) => Res::Ok,
        Err(e) => Res::Error(e.to_string())
    }
        }
    }
    
}

#[rustler::nif]
fn gen_cert_pem(resource: ResourceArc<NifCaResource>, host: String) -> String {
    resource.write().dynamic_gen_cert_pem(&host)
}
     
#[rustler::nif]
fn list_cert_pem(resource: ResourceArc<NifCaResource>) -> Vec<(String, String)>{
    resource.read().list_cert_pem()
}
#[rustler::nif]
fn list_ca_file(resource: ResourceArc<NifCaResource>) -> (String, String){
    resource.read().list_ca_file()
}

rustler::init!("ca",
    [ 
    new,
     gen_ca
    , gen_cert_pem
    , list_cert_pem
    , list_ca_file
    , get_cainfo
    ],
    load = on_load
);

#[cfg(test)]
mod tests {
    use crate::add;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
