#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]

use lura_diagnostic::DiagnosticDb;
use lura_syntax::ParseDb;
use package::HasManifest;
use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = HirDb)]
pub struct Jar(
    package::Package,
    package::Package_all_files,
    resolve::Definition,
    resolve::DefinitionId,
    resolve::Definition_location,
    resolve::Definition_to_string,
    resolve::Reference,
    resolve::Reference_is_type_level,
    resolve::Reference_name,
    resolve::find_constructor,
    resolve::find_function,
    resolve::find_type,
    resolve::query_module,
    resolve::references,
    source::HirSource,
    source::HirError,
    source::HirPath,
    source::HirLocation,
    source::VirtualPath,
    source::new_path,
    source::HirPath_to_string,
    source::Identifier,
    source::top_level::Clause,
    source::top_level::Signature,
    source::top_level::UsingTopLevel,
    source::top_level::ClassDecl,
    source::top_level::TraitDecl,
    source::top_level::DataDecl,
    source::top_level::TypeDecl,
    source::top_level::Constructor,
    source::top_level::BindingGroup,
    source::top_level::CommandTopLevel,
    source::pattern::ConstructorPattern,
    source::pattern::BindingPattern,
    source::declaration::Parameter,
    source::declaration::DocString,
    source::declaration::Attribute,
    source::stmt::LetStmt,
    source::stmt::Block,
    source::stmt::AskStmt,
    source::expr::AbsExpr,
    source::expr::AnnExpr,
    source::expr::CallExpr,
    source::expr::MatchExpr,
    source::type_rep::AppTypeRep,
    source::type_rep::ArrowTypeRep,
    source::type_rep::QPath,
    lower::hir_declare,
    lower::hir_lower,
    completions::completions,
    reparse::reparse_hir_path,
);

/// The database that stores all the information about the source code. It is
/// implemented using the `salsa` crate.
///
/// The `salsa` crate is a crate that provides an incremental and parallel
/// recomputation library. It is used to implement the incremental and parallel
/// compilation of Lura.
pub trait HirDb: HasManifest + ParseDb + DiagnosticDb + DbWithJar<Jar> {}

impl<DB: HasManifest> HirDb for DB where DB: ?Sized + ParseDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

pub mod completions;
pub mod debug;
pub mod lower;
pub mod package;
pub mod reference;
pub mod reparse;
pub mod resolve;
pub mod scope;
pub mod source;
pub mod walking;
