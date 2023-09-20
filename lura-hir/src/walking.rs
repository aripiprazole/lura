//! Defines a module for walking throughout the AST, searching all fields for
//! a given pattern.

use std::collections::HashSet;

use fxhash::FxBuildHasher;

use crate::{
  solver::Reference,
  source::{type_rep::TypeReference, *},
};

pub trait Walker {
  fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T);
}

impl<T: Walker> Walker for Vec<T> {
  fn accept<U: HirListener>(self, db: &dyn crate::HirDb, listener: &mut U) {
    for item in self {
      item.accept(db, listener);
    }
  }
}

impl<T: Walker> Walker for Option<T> {
  fn accept<U: HirListener>(self, db: &dyn crate::HirDb, listener: &mut U) {
    if let Some(item) = self {
      item.accept(db, listener);
    }
  }
}

impl<T: Walker> Walker for HashSet<T, FxBuildHasher> {
  fn accept<U: HirListener>(self, db: &dyn crate::HirDb, listener: &mut U) {
    for item in self {
      item.accept(db, listener);
    }
  }
}

impl<T: Walker> Walker for Box<T> {
  fn accept<U: HirListener>(self, db: &dyn crate::HirDb, listener: &mut U) {
    (*self).accept(db, listener)
  }
}

/// A listener that is called when a node is visited. It does have two methods:
/// `enter` and `exit`. The `enter` method is called when the node is visited
/// and the `exit` method is called when the node is left.
///
/// All functions in this trait have a default implementation that does nothing,
/// so all the functions have the `()` return type.
#[allow(dead_code, unused_variables, clippy::boxed_local)]
#[rustfmt::skip]
pub trait HirListener {
    // SECTION: visitors
    fn visit_reference(&mut self, reference: Reference) {}

    // SECTION: type_rep
    fn visit_unit_type_rep(&mut self) {}
    fn visit_hole_type_rep(&mut self) {}
    fn visit_self_type_rep(&mut self) {}
    fn visit_tt_type_rep(&mut self) {}
    fn enter_arrow_type_rep(&mut self, arrow: type_rep::ArrowTypeRep) {}
    fn enter_error_type_rep(&mut self, error: HirError) {}
    fn enter_path_type_rep(&mut self, definition: TypeReference) {}
    fn enter_qpath_type_rep(&mut self, qpath: type_rep::QPath) {}
    fn enter_app_type_rep(&mut self, app: type_rep::AppTypeRep) {}
    fn enter_downgrade_type_rep(&mut self, expr: Box<expr::Expr>) {}

    // SECTION: expr
    fn visit_empty_expr(&mut self) {}
    fn enter_error_expr(&mut self, error: HirError) {}
    fn enter_path_expr(&mut self, definition: Reference) {}
    fn enter_literal_expr(&mut self, literal: Spanned<literal::Literal>) {}
    fn enter_call_expr(&mut self, call_expr: expr::CallExpr) {}
    fn enter_ann_expr(&mut self, call_expr: expr::AnnExpr) {}
    fn enter_abs_expr(&mut self, call_expr: expr::AbsExpr) {}
    fn enter_match_expr(&mut self, match_expr: expr::MatchExpr) {}
    fn enter_upgrade_expr(&mut self, type_rep: Box<type_rep::TypeRep>) {}

    // SECTION: stmt
    fn visit_empty_stmt(&mut self) {}
    fn enter_error_stmt(&mut self, error: HirError) {}
    fn enter_let_stmt(&mut self, let_stmt: stmt::LetStmt) {}
    fn enter_ask_stmt(&mut self, ask_stmt: stmt::AskStmt) {}
    fn enter_downgrade_stmt(&mut self, expr: expr::Expr) {}
    fn enter_block(&mut self, block: stmt::Block) {}

    // SECTION: pattern
    fn visit_empty_pattern(&mut self) {}
    fn enter_literal_pattern(&mut self, literal: Spanned<literal::Literal>) {}
    fn enter_wildcard_pattern(&mut self, location: Location) {}
    fn enter_rest_pattern(&mut self, location: Location) {}
    fn enter_error_pattern(&mut self, error: HirError) {}
    fn enter_constructor_pattern(&mut self, constructor: pattern::ConstructorPattern) {}
    fn enter_binding_pattern(&mut self, binding: pattern::BindingPattern) {}

    // SECTION: top_level
    fn enter_error_top_level(&mut self, error: HirError) {}
    fn enter_using_top_level(&mut self, using: top_level::UsingTopLevel) {}
    fn enter_binding_top_level(&mut self, binding: top_level::BindingGroup) {}
    fn enter_command_top_level(&mut self, command: top_level::CommandTopLevel) {}
    fn enter_class_top_level(&mut self, class: top_level::ClassDecl) {}
    fn enter_type_top_level(&mut self, type_: top_level::TypeDecl) {}
    fn enter_instance_top_level(&mut self, trait_: top_level::InstanceDecl) {}
    fn enter_trait_top_level(&mut self, trait_: top_level::TraitDecl) {}
    fn enter_data_top_level(&mut self, data: top_level::DataDecl) {}

    // SECTION: type_rep
    fn exit_arrow_type_rep(&mut self, arrow: type_rep::ArrowTypeRep) {}
    fn exit_error_type_rep(&mut self, error: HirError) {}
    fn exit_path_type_rep(&mut self, definition: TypeReference) {}
    fn exit_qpath_type_rep(&mut self, qpath: type_rep::QPath) {}
    fn exit_app_type_rep(&mut self, app: type_rep::AppTypeRep) {}
    fn exit_downgrade_type_rep(&mut self, expr: Box<expr::Expr>) {}

    // SECTION: expr
    fn exit_error_expr(&mut self, error: HirError) {}
    fn exit_path_expr(&mut self, definition: Reference) {}
    fn exit_literal_expr(&mut self, literal: Spanned<literal::Literal>) {}
    fn exit_call_expr(&mut self, call_expr: expr::CallExpr) {}
    fn exit_ann_expr(&mut self, call_expr: expr::AnnExpr) {}
    fn exit_abs_expr(&mut self, call_expr: expr::AbsExpr) {}
    fn exit_match_expr(&mut self, match_expr: expr::MatchExpr) {}
    fn exit_upgrade_expr(&mut self, type_rep: Box<type_rep::TypeRep>) {}

    // SECTION: stmt
    fn exit_error_stmt(&mut self, error: HirError) {}
    fn exit_let_stmt(&mut self, let_stmt: stmt::LetStmt) {}
    fn exit_ask_stmt(&mut self, ask_stmt: stmt::AskStmt) {}
    fn exit_downgrade_stmt(&mut self, expr: expr::Expr) {}
    fn exit_block(&mut self, block: stmt::Block) {}

    // SECTION: pattern
    fn exit_literal_pattern(&mut self, literal: Spanned<literal::Literal>) {}
    fn exit_wildcard_pattern(&mut self, location: Location) {}
    fn exit_rest_pattern(&mut self, location: Location) {}
    fn exit_error_pattern(&mut self, error: HirError) {}
    fn exit_constructor_pattern(&mut self, constructor: pattern::ConstructorPattern) {}
    fn exit_binding_pattern(&mut self, binding: pattern::BindingPattern) {}

    // SECTION: top_level
    fn exit_error_top_level(&mut self, error: HirError) {}
    fn exit_using_top_level(&mut self, using: top_level::UsingTopLevel) {}
    fn exit_binding_top_level(&mut self, binding: top_level::BindingGroup) {}
    fn exit_command_top_level(&mut self, command: top_level::CommandTopLevel) {}
    fn exit_class_top_level(&mut self, class: top_level::ClassDecl) {}
    fn exit_type_top_level(&mut self, class: top_level::TypeDecl) {}
    fn exit_instance_top_level(&mut self, trait_: top_level::InstanceDecl) {}
    fn exit_trait_top_level(&mut self, trait_: top_level::TraitDecl) {}
    fn exit_data_top_level(&mut self, data: top_level::DataDecl) {}
}
