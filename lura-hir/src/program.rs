#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub file_name: String,

    #[return_ref]
    pub source_text: String,
}
