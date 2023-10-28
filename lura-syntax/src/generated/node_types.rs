#[doc = "Typed node `ann_expr`\n\nThis node has these fields:\n- `against`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n- `value`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct AnnExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> AnnExpr<'tree> {
  #[doc = "Get the field `against` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn against (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("against") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for AnnExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "ann_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for AnnExpr<'tree> {
  const KIND: &'static str = "ann_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `app_expr`\n\nThis node has these fields:\n- `argument`: `primary*` ([Primary])\n- `callee`: `primary` ([Primary])\n\nAnd an additional (optional) child: `block?` ([Block])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct AppExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> AppExpr<'tree> {
  #[doc = "Get the field `argument` which has kind `primary*` ([Primary])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Primary<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("argument", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Primary<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `callee` which has kind `primary` ([Primary])"]
  #[allow(dead_code)]
  #[inline]
  pub fn callee(&self) -> type_sitter_lib::NodeResult<'tree, Primary<'tree>> {
    self . 0 . child_by_field_name ("callee") . map (< Primary < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the node's named children"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]
  pub fn children<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl ExactSizeIterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Block_Primary<'tree>>,
    >,
  > + 'a {
    self.0.named_children(c).map(|n| {
      <type_sitter_lib::ExtraOr<'tree, anon_unions::Block_Primary<'tree>> as TryFrom<_>>::try_from(
        n,
      )
    })
  }

  #[doc = "Get the node's named child #i"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
    i: usize,
  ) -> Option<
    type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Block_Primary<'tree>>,
    >,
  > {
    self.0.named_child(i).map(
      <type_sitter_lib::ExtraOr<'tree, anon_unions::Block_Primary<'tree>> as TryFrom<_>>::try_from,
    )
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for AppExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "app_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for AppExpr<'tree> {
  const KIND: &'static str = "app_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `array_expr`\n\nThis node has these fields:\n- `item`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ArrayExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ArrayExpr<'tree> {
  #[doc = "Get the field `item` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn items < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl Iterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > >> + 'a{
    self . 0 . children_by_field_name ("item" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > as TryFrom < _ >> :: try_from (n))
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ArrayExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "array_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ArrayExpr<'tree> {
  const KIND: &'static str = "array_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `ask_stmt`\n\nThis node has these fields:\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n- `value`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct AskStmt<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> AskStmt<'tree> {
  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn pattern(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
  > {
    self . 0 . child_by_field_name ("pattern") . map (< anon_unions :: ConsPattern_GroupPattern_Literal_RestPattern < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for AskStmt<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "ask_stmt" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for AskStmt<'tree> {
  const KIND: &'static str = "ask_stmt";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `attribute`\n\nThis node has these fields:\n- `argument`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n- `name`: `path` ([Path])\n\nAnd an additional (optional) child: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}?`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Attribute<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Attribute<'tree> {
  #[doc = "Get the field `argument` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn arguments < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl Iterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > >> + 'a{
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the node's named children"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]  pub fn children < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl ExactSizeIterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path < 'tree > > >> + 'a{
    self . 0 . named_children (c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the node's named child #i"]
  #[allow(dead_code)]
  #[inline]  pub fn child (& self , i : usize) -> Option < type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path < 'tree > > >>{
    self . 0 . named_child (i) . map (< type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path < 'tree > > as TryFrom < _ >> :: try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Attribute<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "attribute" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Attribute<'tree> {
  const KIND: &'static str = "attribute";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `binary`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Binary<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Binary<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Binary<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "binary" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Binary<'tree> {
  const KIND: &'static str = "binary";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `binary_expr`\n\nThis node has these fields:\n- `lhs`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n- `op`: `infix_op` ([InfixOp])\n- `rhs`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct BinaryExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> BinaryExpr<'tree> {
  #[doc = "Get the field `lhs` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn lhs(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("lhs") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `op` which has kind `infix_op` ([InfixOp])"]
  #[allow(dead_code)]
  #[inline]
  pub fn op(&self) -> type_sitter_lib::NodeResult<'tree, InfixOp<'tree>> {
    self . 0 . child_by_field_name ("op") . map (< InfixOp < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `rhs` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn rhs(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("rhs") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for BinaryExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "binary_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for BinaryExpr<'tree> {
  const KIND: &'static str = "binary_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `block`\n\nThis node has these fields:\n- `statement`: `{ask_stmt | expr_stmt | if_stmt | let_stmt}*` ([anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Block<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Block<'tree> {
  #[doc = "Get the field `statement` which has kind `{ask_stmt | expr_stmt | if_stmt | let_stmt}*` ([anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt])"]
  #[allow(dead_code)]
  #[inline]
  pub fn statements<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("statement" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AskStmt_ExprStmt_IfStmt_LetStmt < 'tree > > as TryFrom < _ >> :: try_from (n))
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Block<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "block" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Block<'tree> {
  const KIND: &'static str = "block";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `class_decl`\n\nThis node has these fields:\n- `argument`: `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])\n- `attribute`: `attribute*` ([Attribute])\n- `class_body`: `{signature | { | }}*` ([anon_unions::Signature_LBrace_RBrace])\n- `clause_type`: `clause_type?` ([ClauseType])\n- `doc_string`: `doc_string*` ([DocString])\n- `field`: `signature*` ([Signature])\n- `name`: `path` ([Path])\n- `visibility`: `visibility?` ([Visibility])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ClassDecl<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ClassDecl<'tree> {
  #[doc = "Get the field `argument` which has kind `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ExplicitArguments_ImplicitArguments<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ExplicitArguments_ImplicitArguments < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `class_body` which has kind `{signature | { | }}*` ([anon_unions::Signature_LBrace_RBrace])"]
  #[allow(dead_code)]
  #[inline]
  pub fn class_bodys<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Signature_LBrace_RBrace<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("class_body" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Signature_LBrace_RBrace < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `clause_type` which has kind `clause_type?` ([ClauseType])"]
  #[allow(dead_code)]
  #[inline]
  pub fn clause_type(&self) -> Option<type_sitter_lib::NodeResult<'tree, ClauseType<'tree>>> {
    self
      .0
      .child_by_field_name("clause_type")
      .map(<ClauseType<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `field` which has kind `signature*` ([Signature])"]
  #[allow(dead_code)]
  #[inline]
  pub fn fields<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Signature<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("field", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Signature<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `visibility` which has kind `visibility?` ([Visibility])"]
  #[allow(dead_code)]
  #[inline]
  pub fn visibility(&self) -> Option<type_sitter_lib::NodeResult<'tree, Visibility<'tree>>> {
    self
      .0
      .child_by_field_name("visibility")
      .map(<Visibility<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ClassDecl<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "class_decl" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ClassDecl<'tree> {
  const KIND: &'static str = "class_decl";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `clause`\n\nThis node has these fields:\n- `attribute`: `attribute*` ([Attribute])\n- `doc_string`: `doc_string*` ([DocString])\n- `name`: `path` ([Path])\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}*` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n- `value`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}?` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Clause<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Clause<'tree> {
  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}*` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn patterns<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
      >,
    >,
  > + 'a {
    self.0.children_by_field_name("pattern", c).map(|n| {
      <type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
      > as TryFrom<_>>::try_from(n)
    })
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}?` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(
    &self,
  ) -> Option<
    type_sitter_lib::NodeResult<
      'tree,
      anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
        'tree,
      >,
    >,
  > {
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Clause<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "clause" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Clause<'tree> {
  const KIND: &'static str = "clause";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `clause_type`\n\nThis node has these fields:\n- `clause_type`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ClauseType<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ClauseType<'tree> {
  #[doc = "Get the field `clause_type` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn clause_type (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("clause_type") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ClauseType<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "clause_type" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ClauseType<'tree> {
  const KIND: &'static str = "clause_type";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `command`\n\nThis node has these fields:\n- `argument`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n- `attribute`: `attribute*` ([Attribute])\n- `command`: `path` ([Path])\n- `doc_string`: `doc_string*` ([DocString])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Command<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Command<'tree> {
  #[doc = "Get the field `argument` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn arguments < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl Iterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > >> + 'a{
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `command` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn command(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("command") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Command<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "command" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Command<'tree> {
  const KIND: &'static str = "command";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `cons_pattern`\n\nThis node has these fields:\n- `name`: `path` ([Path])\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}*` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ConsPattern<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ConsPattern<'tree> {
  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}*` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn patterns<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
      >,
    >,
  > + 'a {
    self.0.children_by_field_name("pattern", c).map(|n| {
      <type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
      > as TryFrom<_>>::try_from(n)
    })
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ConsPattern<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "cons_pattern" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ConsPattern<'tree> {
  const KIND: &'static str = "cons_pattern";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `data_decl`\n\nThis node has these fields:\n- `argument`: `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])\n- `attribute`: `attribute*` ([Attribute])\n- `clause_type`: `clause_type?` ([ClauseType])\n- `constructor`: `{, | function_constructor | signature_constructor}*` ([anon_unions::Comma_FunctionConstructor_SignatureConstructor])\n- `data_body`: `{, | ; | function_constructor | signature | signature_constructor | { | }}*` ([anon_unions::Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace])\n- `doc_string`: `doc_string*` ([DocString])\n- `field`: `signature*` ([Signature])\n- `method`: `signature*` ([Signature])\n- `name`: `path` ([Path])\n- `visibility`: `visibility?` ([Visibility])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct DataDecl<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> DataDecl<'tree> {
  #[doc = "Get the field `argument` which has kind `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ExplicitArguments_ImplicitArguments<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ExplicitArguments_ImplicitArguments < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `clause_type` which has kind `clause_type?` ([ClauseType])"]
  #[allow(dead_code)]
  #[inline]
  pub fn clause_type(&self) -> Option<type_sitter_lib::NodeResult<'tree, ClauseType<'tree>>> {
    self
      .0
      .child_by_field_name("clause_type")
      .map(<ClauseType<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `constructor` which has kind `{, | function_constructor | signature_constructor}*` ([anon_unions::Comma_FunctionConstructor_SignatureConstructor])"]
  #[allow(dead_code)]
  #[inline]
  pub fn constructors<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::Comma_FunctionConstructor_SignatureConstructor<'tree>,
      >,
    >,
  > + 'a {
    self.0.children_by_field_name("constructor", c).map(|n| {
      <type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::Comma_FunctionConstructor_SignatureConstructor<'tree>,
      > as TryFrom<_>>::try_from(n)
    })
  }

  #[doc = "Get the field `data_body` which has kind `{, | ; | function_constructor | signature | signature_constructor | { | }}*` ([anon_unions::Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace])"]
  #[allow(dead_code)]
  #[inline]  pub fn data_bodys < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl Iterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace < 'tree > > >> + 'a{
    self . 0 . children_by_field_name ("data_body" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `field` which has kind `signature*` ([Signature])"]
  #[allow(dead_code)]
  #[inline]
  pub fn fields<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Signature<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("field", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Signature<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `method` which has kind `signature*` ([Signature])"]
  #[allow(dead_code)]
  #[inline]
  pub fn methods<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Signature<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("method", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Signature<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `visibility` which has kind `visibility?` ([Visibility])"]
  #[allow(dead_code)]
  #[inline]
  pub fn visibility(&self) -> Option<type_sitter_lib::NodeResult<'tree, Visibility<'tree>>> {
    self
      .0
      .child_by_field_name("visibility")
      .map(<Visibility<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for DataDecl<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "data_decl" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for DataDecl<'tree> {
  const KIND: &'static str = "data_decl";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `doc_string`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct DocString<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> DocString<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for DocString<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "doc_string" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for DocString<'tree> {
  const KIND: &'static str = "doc_string";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `explicit_arguments`\n\nThis node has these fields:\n- `parameter`: `{forall_parameter | parameter}*` ([anon_unions::ForallParameter_Parameter])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ExplicitArguments<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ExplicitArguments<'tree> {
  #[doc = "Get the field `parameter` which has kind `{forall_parameter | parameter}*` ([anon_unions::ForallParameter_Parameter])"]
  #[allow(dead_code)]
  #[inline]
  pub fn parameters<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ForallParameter_Parameter<'tree>>,
    >,
  > + 'a {
    self.0.children_by_field_name("parameter", c).map(|n| {
      <type_sitter_lib::ExtraOr<'tree, anon_unions::ForallParameter_Parameter<'tree>> as TryFrom<
        _,
      >>::try_from(n)
    })
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ExplicitArguments<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "explicit_arguments" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ExplicitArguments<'tree> {
  const KIND: &'static str = "explicit_arguments";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `expr_stmt`\n\nThis node has a child: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ExprStmt<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ExprStmt<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . named_child (0) . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ExprStmt<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "expr_stmt" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ExprStmt<'tree> {
  const KIND: &'static str = "expr_stmt";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `f32`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct F32<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> F32<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for F32<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "f32" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for F32<'tree> {
  const KIND: &'static str = "f32";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `f64`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct F64<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> F64<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for F64<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "f64" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for F64<'tree> {
  const KIND: &'static str = "f64";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `forall_expr`\n\nThis node has these fields:\n- `parameter`: `forall_parameter+` ([ForallParameter])\n- `value`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ForallExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ForallExpr<'tree> {
  #[doc = "Get the field `parameter` which has kind `forall_parameter+` ([ForallParameter])"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]
  pub fn parameters<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, ForallParameter<'tree>>,
    >,
  > + 'a {
    self
      .0
      .children_by_field_name("parameter", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, ForallParameter<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn value (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ForallExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "forall_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ForallExpr<'tree> {
  const KIND: &'static str = "forall_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `forall_parameter`\n\nThis node has these fields:\n- `identifier`: `identifier` ([Identifier])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ForallParameter<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ForallParameter<'tree> {
  #[doc = "Get the field `identifier` which has kind `identifier` ([Identifier])"]
  #[allow(dead_code)]
  #[inline]
  pub fn identifier(&self) -> type_sitter_lib::NodeResult<'tree, Identifier<'tree>> {
    self . 0 . child_by_field_name ("identifier") . map (< Identifier < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ForallParameter<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "forall_parameter" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ForallParameter<'tree> {
  const KIND: &'static str = "forall_parameter";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `free_variable`\n\nThis node has a child: `identifier` ([Identifier])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct FreeVariable<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> FreeVariable<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(&self) -> type_sitter_lib::NodeResult<'tree, Identifier<'tree>> {
    self . 0 . named_child (0) . map (< Identifier < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for FreeVariable<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "free_variable" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for FreeVariable<'tree> {
  const KIND: &'static str = "free_variable";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `function_constructor`\n\nThis node has these fields:\n- `attribute`: `attribute*` ([Attribute])\n- `doc_string`: `doc_string*` ([DocString])\n- `name`: `path` ([Path])\n- `parameter`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}*` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct FunctionConstructor<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> FunctionConstructor<'tree> {
  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `parameter` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}*` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn parameters < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl Iterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > > >> + 'a{
    self . 0 . children_by_field_name ("parameter" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > > as TryFrom < _ >> :: try_from (n))
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for FunctionConstructor<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "function_constructor" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for FunctionConstructor<'tree> {
  const KIND: &'static str = "function_constructor";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `group_pattern`\n\nThis node has these fields:\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct GroupPattern<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> GroupPattern<'tree> {
  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn pattern(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
  > {
    self . 0 . child_by_field_name ("pattern") . map (< anon_unions :: ConsPattern_GroupPattern_Literal_RestPattern < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for GroupPattern<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "group_pattern" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for GroupPattern<'tree> {
  const KIND: &'static str = "group_pattern";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `hex`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Hex<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Hex<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Hex<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "hex" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Hex<'tree> {
  const KIND: &'static str = "hex";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `i128`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct I128<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> I128<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I128<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "i128" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for I128<'tree> {
  const KIND: &'static str = "i128";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `i16`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct I16<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> I16<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I16<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "i16" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for I16<'tree> {
  const KIND: &'static str = "i16";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `i64`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct I64<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> I64<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I64<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "i64" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for I64<'tree> {
  const KIND: &'static str = "i64";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `i8`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct I8<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> I8<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I8<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "i8" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for I8<'tree> {
  const KIND: &'static str = "i8";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `identifier`\n\nThis node has a child: `{simple_identifier | symbol_identifier}`:\n- [SimpleIdentifier]\n- [SymbolIdentifier]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Identifier<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Identifier<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> type_sitter_lib::NodeResult<'tree, anon_unions::SimpleIdentifier_SymbolIdentifier<'tree>> {
    self . 0 . named_child (0) . map (< anon_unions :: SimpleIdentifier_SymbolIdentifier < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Identifier<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "identifier" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Identifier<'tree> {
  const KIND: &'static str = "identifier";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `if_expr`\n\nThis node has these fields:\n- `condition`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n- `otherwise`: `otherwise_body` ([OtherwiseBody])\n- `then`: `then_body` ([ThenBody])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct IfExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> IfExpr<'tree> {
  #[doc = "Get the field `condition` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn condition(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("condition") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `otherwise` which has kind `otherwise_body` ([OtherwiseBody])"]
  #[allow(dead_code)]
  #[inline]
  pub fn otherwise(&self) -> type_sitter_lib::NodeResult<'tree, OtherwiseBody<'tree>> {
    self . 0 . child_by_field_name ("otherwise") . map (< OtherwiseBody < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `then` which has kind `then_body` ([ThenBody])"]
  #[allow(dead_code)]
  #[inline]
  pub fn then(&self) -> type_sitter_lib::NodeResult<'tree, ThenBody<'tree>> {
    self . 0 . child_by_field_name ("then") . map (< ThenBody < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for IfExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "if_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for IfExpr<'tree> {
  const KIND: &'static str = "if_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `if_stmt`\n\nThis node has these fields:\n- `condition`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n- `otherwise`: `otherwise_body?` ([OtherwiseBody])\n- `then`: `then_body` ([ThenBody])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct IfStmt<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> IfStmt<'tree> {
  #[doc = "Get the field `condition` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn condition(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("condition") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `otherwise` which has kind `otherwise_body?` ([OtherwiseBody])"]
  #[allow(dead_code)]
  #[inline]
  pub fn otherwise(&self) -> Option<type_sitter_lib::NodeResult<'tree, OtherwiseBody<'tree>>> {
    self
      .0
      .child_by_field_name("otherwise")
      .map(<OtherwiseBody<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `then` which has kind `then_body` ([ThenBody])"]
  #[allow(dead_code)]
  #[inline]
  pub fn then(&self) -> type_sitter_lib::NodeResult<'tree, ThenBody<'tree>> {
    self . 0 . child_by_field_name ("then") . map (< ThenBody < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for IfStmt<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "if_stmt" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for IfStmt<'tree> {
  const KIND: &'static str = "if_stmt";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `implicit_arguments`\n\nThis node has these fields:\n- `parameter`: `{forall_parameter | parameter}*` ([anon_unions::ForallParameter_Parameter])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ImplicitArguments<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ImplicitArguments<'tree> {
  #[doc = "Get the field `parameter` which has kind `{forall_parameter | parameter}*` ([anon_unions::ForallParameter_Parameter])"]
  #[allow(dead_code)]
  #[inline]
  pub fn parameters<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ForallParameter_Parameter<'tree>>,
    >,
  > + 'a {
    self.0.children_by_field_name("parameter", c).map(|n| {
      <type_sitter_lib::ExtraOr<'tree, anon_unions::ForallParameter_Parameter<'tree>> as TryFrom<
        _,
      >>::try_from(n)
    })
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ImplicitArguments<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "implicit_arguments" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ImplicitArguments<'tree> {
  const KIND: &'static str = "implicit_arguments";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `infix_op`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct InfixOp<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> InfixOp<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for InfixOp<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "infix_op" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for InfixOp<'tree> {
  const KIND: &'static str = "infix_op";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `instance_decl`\n\nThis node has these fields:\n- `argument`: `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])\n- `attribute`: `attribute*` ([Attribute])\n- `doc_string`: `doc_string*` ([DocString])\n- `field`: `signature*` ([Signature])\n- `item`: `primary+` ([Primary])\n- `name`: `path` ([Path])\n- `trait_body`: `{signature | { | }}*` ([anon_unions::Signature_LBrace_RBrace])\n- `visibility`: `visibility?` ([Visibility])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct InstanceDecl<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> InstanceDecl<'tree> {
  #[doc = "Get the field `argument` which has kind `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ExplicitArguments_ImplicitArguments<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ExplicitArguments_ImplicitArguments < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `field` which has kind `signature*` ([Signature])"]
  #[allow(dead_code)]
  #[inline]
  pub fn fields<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Signature<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("field", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Signature<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `item` which has kind `primary+` ([Primary])"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]
  pub fn items<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Primary<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("item", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Primary<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `trait_body` which has kind `{signature | { | }}*` ([anon_unions::Signature_LBrace_RBrace])"]
  #[allow(dead_code)]
  #[inline]
  pub fn trait_bodys<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Signature_LBrace_RBrace<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("trait_body" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Signature_LBrace_RBrace < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `visibility` which has kind `visibility?` ([Visibility])"]
  #[allow(dead_code)]
  #[inline]
  pub fn visibility(&self) -> Option<type_sitter_lib::NodeResult<'tree, Visibility<'tree>>> {
    self
      .0
      .child_by_field_name("visibility")
      .map(<Visibility<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for InstanceDecl<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "instance_decl" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for InstanceDecl<'tree> {
  const KIND: &'static str = "instance_decl";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `lam_expr`\n\nThis node has these fields:\n- `parameter`: `{, | forall_parameter | parameter}*` ([anon_unions::Comma_ForallParameter_Parameter])\n- `value`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct LamExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> LamExpr<'tree> {
  #[doc = "Get the field `parameter` which has kind `{, | forall_parameter | parameter}*` ([anon_unions::Comma_ForallParameter_Parameter])"]
  #[allow(dead_code)]
  #[inline]
  pub fn parameters<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Comma_ForallParameter_Parameter<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("parameter" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Comma_ForallParameter_Parameter < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LamExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "lam_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for LamExpr<'tree> {
  const KIND: &'static str = "lam_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `let_stmt`\n\nThis node has these fields:\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n- `value`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct LetStmt<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> LetStmt<'tree> {
  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn pattern(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
  > {
    self . 0 . child_by_field_name ("pattern") . map (< anon_unions :: ConsPattern_GroupPattern_Literal_RestPattern < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LetStmt<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "let_stmt" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for LetStmt<'tree> {
  const KIND: &'static str = "let_stmt";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `line_comment`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct LineComment<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> LineComment<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LineComment<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "line_comment" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for LineComment<'tree> {
  const KIND: &'static str = "line_comment";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `literal`\n\nThis node has a child: `{char | f32 | f64 | i128 | i16 | i64 | i8 | nat | string | u1 | u128 | u16 | u32 | u64 | u8}`:\n- [Char]\n- [F32]\n- [F64]\n- [I128]\n- [I16]\n- [I64]\n- [I8]\n- [Nat]\n- [String]\n- [U1]\n- [U128]\n- [U16]\n- [U32]\n- [U64]\n- [U8]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Literal<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Literal<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8<'tree>,
  > {
    self . 0 . named_child (0) . map (< anon_unions :: Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8 < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Literal<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "literal" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Literal<'tree> {
  const KIND: &'static str = "literal";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `match_arm`\n\nThis node has these fields:\n- `body`: `{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct MatchArm<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> MatchArm<'tree> {
  #[doc = "Get the field `body` which has kind `{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn body (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > >{
    self . 0 . child_by_field_name ("body") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn pattern(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
  > {
    self . 0 . child_by_field_name ("pattern") . map (< anon_unions :: ConsPattern_GroupPattern_Literal_RestPattern < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for MatchArm<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "match_arm" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for MatchArm<'tree> {
  const KIND: &'static str = "match_arm";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `match_expr`\n\nThis node has these fields:\n- `arm`: `match_arm*` ([MatchArm])\n- `scrutinee`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct MatchExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> MatchExpr<'tree> {
  #[doc = "Get the field `arm` which has kind `match_arm*` ([MatchArm])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arms<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, MatchArm<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("arm", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, MatchArm<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `scrutinee` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn scrutinee(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >,
  > {
    self . 0 . child_by_field_name ("scrutinee") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for MatchExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "match_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for MatchExpr<'tree> {
  const KIND: &'static str = "match_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `nat`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Nat<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Nat<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Nat<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "nat" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Nat<'tree> {
  const KIND: &'static str = "nat";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `octal`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Octal<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Octal<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Octal<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "octal" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Octal<'tree> {
  const KIND: &'static str = "octal";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `otherwise_body`\n\nThis node has these fields:\n- `value`: `{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct OtherwiseBody<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> OtherwiseBody<'tree> {
  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn value (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > >{
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for OtherwiseBody<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "otherwise_body" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for OtherwiseBody<'tree> {
  const KIND: &'static str = "otherwise_body";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `parameter`\n\nThis node has these fields:\n- `parameter_type`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n- `pattern`: `{cons_pattern | group_pattern | literal | rest_pattern}?` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Parameter<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Parameter<'tree> {
  #[doc = "Get the field `parameter_type` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn parameter_type (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("parameter_type") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `pattern` which has kind `{cons_pattern | group_pattern | literal | rest_pattern}?` ([anon_unions::ConsPattern_GroupPattern_Literal_RestPattern])"]
  #[allow(dead_code)]
  #[inline]
  pub fn pattern(
    &self,
  ) -> Option<
    type_sitter_lib::NodeResult<
      'tree,
      anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>,
    >,
  > {
    self.0.child_by_field_name("pattern").map(
      <anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree> as TryFrom<_>>::try_from,
    )
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Parameter<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "parameter" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Parameter<'tree> {
  const KIND: &'static str = "parameter";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `path`\n\nThis node has these fields:\n- `segment`: `identifier+` ([Identifier])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Path<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Path<'tree> {
  #[doc = "Get the field `segment` which has kind `identifier+` ([Identifier])"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]
  pub fn segments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Identifier<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("segment", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Identifier<'tree>> as TryFrom<_>>::try_from(n))
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Path<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "path" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Path<'tree> {
  const KIND: &'static str = "path";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `pi_expr`\n\nThis node has these fields:\n- `parameter`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | pi_named_parameter_set | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr])\n- `value`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct PiExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> PiExpr<'tree> {
  #[doc = "Get the field `parameter` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | pi_named_parameter_set | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn parameter (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("parameter") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn value (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for PiExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "pi_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for PiExpr<'tree> {
  const KIND: &'static str = "pi_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `pi_named_parameter_set`\n\nThis node has these fields:\n- `parameter`: `{, | forall_parameter | parameter}+` ([anon_unions::Comma_ForallParameter_Parameter])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct PiNamedParameterSet<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> PiNamedParameterSet<'tree> {
  #[doc = "Get the field `parameter` which has kind `{, | forall_parameter | parameter}+` ([anon_unions::Comma_ForallParameter_Parameter])"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]
  pub fn parameters<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Comma_ForallParameter_Parameter<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("parameter" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Comma_ForallParameter_Parameter < 'tree > > as TryFrom < _ >> :: try_from (n))
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for PiNamedParameterSet<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "pi_named_parameter_set" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for PiNamedParameterSet<'tree> {
  const KIND: &'static str = "pi_named_parameter_set";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `primary`\n\nThis node has a child: `{array_expr | free_variable | if_expr | literal | match_expr | path | return_expr | tuple_expr}`:\n- [ArrayExpr]\n- [FreeVariable]\n- [IfExpr]\n- [Literal]\n- [MatchExpr]\n- [Path]\n- [ReturnExpr]\n- [TupleExpr]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Primary<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Primary<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> type_sitter_lib::NodeResult<
    'tree,
    anon_unions::ArrayExpr_FreeVariable_IfExpr_Literal_MatchExpr_Path_ReturnExpr_TupleExpr<'tree>,
  > {
    self . 0 . named_child (0) . map (< anon_unions :: ArrayExpr_FreeVariable_IfExpr_Literal_MatchExpr_Path_ReturnExpr_TupleExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Primary<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "primary" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Primary<'tree> {
  const KIND: &'static str = "primary";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `return_expr`\n\nThis node has these fields:\n- `value`: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}?` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ReturnExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ReturnExpr<'tree> {
  #[doc = "Get the field `value` which has kind `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}?` ([anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(
    &self,
  ) -> Option<
    type_sitter_lib::NodeResult<
      'tree,
      anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
        'tree,
      >,
    >,
  > {
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ReturnExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "return_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ReturnExpr<'tree> {
  const KIND: &'static str = "return_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `sigma_expr`\n\nThis node has these fields:\n- `parameter`: `{, | cons_pattern | group_pattern | literal | parameter | rest_pattern}+` ([anon_unions::Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern])\n- `value`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct SigmaExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> SigmaExpr<'tree> {
  #[doc = "Get the field `parameter` which has kind `{, | cons_pattern | group_pattern | literal | parameter | rest_pattern}+` ([anon_unions::Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern])"]
  #[doc = "This is guaranteed to return at least one child"]
  #[allow(dead_code)]
  #[inline]
  pub fn parameters<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern<'tree>,
      >,
    >,
  > + 'a {
    self.0.children_by_field_name("parameter", c).map(|n| {
      <type_sitter_lib::ExtraOr<
        'tree,
        anon_unions::Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern<'tree>,
      > as TryFrom<_>>::try_from(n)
    })
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn value (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SigmaExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "sigma_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for SigmaExpr<'tree> {
  const KIND: &'static str = "sigma_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `signature`\n\nThis node has these fields:\n- `argument`: `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])\n- `attribute`: `attribute*` ([Attribute])\n- `clause_type`: `clause_type?` ([ClauseType])\n- `doc_string`: `doc_string*` ([DocString])\n- `name`: `path` ([Path])\n- `value`: `block?` ([Block])\n- `visibility`: `visibility?` ([Visibility])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Signature<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Signature<'tree> {
  #[doc = "Get the field `argument` which has kind `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ExplicitArguments_ImplicitArguments<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ExplicitArguments_ImplicitArguments < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `clause_type` which has kind `clause_type?` ([ClauseType])"]
  #[allow(dead_code)]
  #[inline]
  pub fn clause_type(&self) -> Option<type_sitter_lib::NodeResult<'tree, ClauseType<'tree>>> {
    self
      .0
      .child_by_field_name("clause_type")
      .map(<ClauseType<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `value` which has kind `block?` ([Block])"]
  #[allow(dead_code)]
  #[inline]
  pub fn value(&self) -> Option<type_sitter_lib::NodeResult<'tree, Block<'tree>>> {
    self
      .0
      .child_by_field_name("value")
      .map(<Block<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `visibility` which has kind `visibility?` ([Visibility])"]
  #[allow(dead_code)]
  #[inline]
  pub fn visibility(&self) -> Option<type_sitter_lib::NodeResult<'tree, Visibility<'tree>>> {
    self
      .0
      .child_by_field_name("visibility")
      .map(<Visibility<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Signature<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "signature" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Signature<'tree> {
  const KIND: &'static str = "signature";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `signature_constructor`\n\nThis node has these fields:\n- `attribute`: `attribute*` ([Attribute])\n- `doc_string`: `doc_string*` ([DocString])\n- `field_type`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n- `name`: `path` ([Path])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct SignatureConstructor<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> SignatureConstructor<'tree> {
  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `field_type` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn field_type (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > >{
    self . 0 . child_by_field_name ("field_type") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SignatureConstructor<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "signature_constructor" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for SignatureConstructor<'tree> {
  const KIND: &'static str = "signature_constructor";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `source_file`\n\nThis node has these fields:\n- `decl`: `{class_decl | clause | command | data_decl | instance_decl | signature | trait_decl | type_decl | using}*` ([anon_unions::ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using])\n- `hash_bang`: `hash_bang?` ([HashBang])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct SourceFile<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> SourceFile<'tree> {
  #[doc = "Get the field `decl` which has kind `{class_decl | clause | command | data_decl | instance_decl | signature | trait_decl | type_decl | using}*` ([anon_unions::ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using])"]
  #[allow(dead_code)]
  #[inline]  pub fn decls < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl Iterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using < 'tree > > >> + 'a{
    self . 0 . children_by_field_name ("decl" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `hash_bang` which has kind `hash_bang?` ([HashBang])"]
  #[allow(dead_code)]
  #[inline]
  pub fn hash_bang(&self) -> Option<type_sitter_lib::NodeResult<'tree, HashBang<'tree>>> {
    self
      .0
      .child_by_field_name("hash_bang")
      .map(<HashBang<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SourceFile<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "source_file" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for SourceFile<'tree> {
  const KIND: &'static str = "source_file";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `symbol_identifier`\n\nThis node has a child: `infix_op` ([InfixOp])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct SymbolIdentifier<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> SymbolIdentifier<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(&self) -> type_sitter_lib::NodeResult<'tree, InfixOp<'tree>> {
    self . 0 . named_child (0) . map (< InfixOp < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SymbolIdentifier<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "symbol_identifier" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for SymbolIdentifier<'tree> {
  const KIND: &'static str = "symbol_identifier";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `then_body`\n\nThis node has a child: `{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [Block]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct ThenBody<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> ThenBody<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]  pub fn child (& self) -> type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > >{
    self . 0 . named_child (0) . map (< anon_unions :: AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ThenBody<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "then_body" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for ThenBody<'tree> {
  const KIND: &'static str = "then_body";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `trait_decl`\n\nThis node has these fields:\n- `argument`: `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])\n- `attribute`: `attribute*` ([Attribute])\n- `clause_type`: `clause_type?` ([ClauseType])\n- `doc_string`: `doc_string*` ([DocString])\n- `field`: `signature*` ([Signature])\n- `name`: `path` ([Path])\n- `trait_body`: `{signature | { | }}*` ([anon_unions::Signature_LBrace_RBrace])\n- `visibility`: `visibility?` ([Visibility])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct TraitDecl<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> TraitDecl<'tree> {
  #[doc = "Get the field `argument` which has kind `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ExplicitArguments_ImplicitArguments<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ExplicitArguments_ImplicitArguments < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `clause_type` which has kind `clause_type?` ([ClauseType])"]
  #[allow(dead_code)]
  #[inline]
  pub fn clause_type(&self) -> Option<type_sitter_lib::NodeResult<'tree, ClauseType<'tree>>> {
    self
      .0
      .child_by_field_name("clause_type")
      .map(<ClauseType<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `field` which has kind `signature*` ([Signature])"]
  #[allow(dead_code)]
  #[inline]
  pub fn fields<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Signature<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("field", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Signature<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `trait_body` which has kind `{signature | { | }}*` ([anon_unions::Signature_LBrace_RBrace])"]
  #[allow(dead_code)]
  #[inline]
  pub fn trait_bodys<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::Signature_LBrace_RBrace<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("trait_body" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: Signature_LBrace_RBrace < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `visibility` which has kind `visibility?` ([Visibility])"]
  #[allow(dead_code)]
  #[inline]
  pub fn visibility(&self) -> Option<type_sitter_lib::NodeResult<'tree, Visibility<'tree>>> {
    self
      .0
      .child_by_field_name("visibility")
      .map(<Visibility<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for TraitDecl<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "trait_decl" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for TraitDecl<'tree> {
  const KIND: &'static str = "trait_decl";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `tuple_expr`\n\nThis node has children: `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}*`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct TupleExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> TupleExpr<'tree> {
  #[doc = "Get the node's named children"]
  #[allow(dead_code)]
  #[inline]  pub fn children < 'a > (& self , c : & 'a mut tree_sitter :: TreeCursor < 'tree >) -> impl ExactSizeIterator < Item = type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > >> + 'a{
    self . 0 . named_children (c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the node's named child #i"]
  #[allow(dead_code)]
  #[inline]  pub fn child (& self , i : usize) -> Option < type_sitter_lib :: NodeResult < 'tree , type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > >>{
    self . 0 . named_child (i) . map (< type_sitter_lib :: ExtraOr < 'tree , anon_unions :: AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr < 'tree > > as TryFrom < _ >> :: try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for TupleExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "tuple_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for TupleExpr<'tree> {
  const KIND: &'static str = "tuple_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `type_app_expr`\n\nThis node has these fields:\n- `argument`: `primary*` ([Primary])\n- `callee`: `primary` ([Primary])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct TypeAppExpr<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> TypeAppExpr<'tree> {
  #[doc = "Get the field `argument` which has kind `primary*` ([Primary])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Primary<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("argument", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Primary<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `callee` which has kind `primary` ([Primary])"]
  #[allow(dead_code)]
  #[inline]
  pub fn callee(&self) -> type_sitter_lib::NodeResult<'tree, Primary<'tree>> {
    self . 0 . child_by_field_name ("callee") . map (< Primary < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for TypeAppExpr<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "type_app_expr" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for TypeAppExpr<'tree> {
  const KIND: &'static str = "type_app_expr";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `type_decl`\n\nThis node has these fields:\n- `argument`: `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])\n- `attribute`: `attribute*` ([Attribute])\n- `clause_type`: `clause_type?` ([ClauseType])\n- `doc_string`: `doc_string*` ([DocString])\n- `name`: `path` ([Path])\n- `value`: `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}?` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])\n- `visibility`: `visibility?` ([Visibility])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct TypeDecl<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> TypeDecl<'tree> {
  #[doc = "Get the field `argument` which has kind `{explicit_arguments | implicit_arguments}*` ([anon_unions::ExplicitArguments_ImplicitArguments])"]
  #[allow(dead_code)]
  #[inline]
  pub fn arguments<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<
      'tree,
      type_sitter_lib::ExtraOr<'tree, anon_unions::ExplicitArguments_ImplicitArguments<'tree>>,
    >,
  > + 'a {
    self . 0 . children_by_field_name ("argument" , c) . map (| n | < type_sitter_lib :: ExtraOr < 'tree , anon_unions :: ExplicitArguments_ImplicitArguments < 'tree > > as TryFrom < _ >> :: try_from (n))
  }

  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `clause_type` which has kind `clause_type?` ([ClauseType])"]
  #[allow(dead_code)]
  #[inline]
  pub fn clause_type(&self) -> Option<type_sitter_lib::NodeResult<'tree, ClauseType<'tree>>> {
    self
      .0
      .child_by_field_name("clause_type")
      .map(<ClauseType<'tree> as TryFrom<_>>::try_from)
  }

  #[doc = "Get the field `doc_string` which has kind `doc_string*` ([DocString])"]
  #[allow(dead_code)]
  #[inline]
  pub fn doc_strings<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, DocString<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("doc_string", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, DocString<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `name` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn name(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("name") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }

  #[doc = "Get the field `value` which has kind `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}?` ([anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr])"]
  #[allow(dead_code)]
  #[inline]  pub fn value (& self) -> Option < type_sitter_lib :: NodeResult < 'tree , anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > > >{
    self . 0 . child_by_field_name ("value") . map (< anon_unions :: AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr < 'tree > as TryFrom < _ >> :: try_from)
  }

  #[doc = "Get the field `visibility` which has kind `visibility?` ([Visibility])"]
  #[allow(dead_code)]
  #[inline]
  pub fn visibility(&self) -> Option<type_sitter_lib::NodeResult<'tree, Visibility<'tree>>> {
    self
      .0
      .child_by_field_name("visibility")
      .map(<Visibility<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for TypeDecl<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "type_decl" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for TypeDecl<'tree> {
  const KIND: &'static str = "type_decl";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `u1`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct U1<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> U1<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U1<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "u1" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for U1<'tree> {
  const KIND: &'static str = "u1";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `u128`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct U128<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> U128<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U128<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "u128" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for U128<'tree> {
  const KIND: &'static str = "u128";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `u16`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct U16<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> U16<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U16<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "u16" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for U16<'tree> {
  const KIND: &'static str = "u16";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `u32`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct U32<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> U32<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U32<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "u32" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for U32<'tree> {
  const KIND: &'static str = "u32";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `u64`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct U64<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> U64<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U64<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "u64" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for U64<'tree> {
  const KIND: &'static str = "u64";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `u8`\n\nThis node has an (optional) child: `{binary | hex | octal}?`:\n- [Binary]\n- [Hex]\n- [Octal]\n\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct U8<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> U8<'tree> {
  #[doc = "Get the node's only named child"]
  #[allow(dead_code)]
  #[inline]
  pub fn child(
    &self,
  ) -> Option<type_sitter_lib::NodeResult<'tree, anon_unions::Binary_Hex_Octal<'tree>>> {
    self
      .0
      .named_child(0)
      .map(<anon_unions::Binary_Hex_Octal<'tree> as TryFrom<_>>::try_from)
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U8<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "u8" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for U8<'tree> {
  const KIND: &'static str = "u8";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `using`\n\nThis node has these fields:\n- `attribute`: `attribute*` ([Attribute])\n- `path`: `path` ([Path])\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Using<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Using<'tree> {
  #[doc = "Get the field `attribute` which has kind `attribute*` ([Attribute])"]
  #[allow(dead_code)]
  #[inline]
  pub fn attributes<'a>(
    &self,
    c: &'a mut tree_sitter::TreeCursor<'tree>,
  ) -> impl Iterator<
    Item = type_sitter_lib::NodeResult<'tree, type_sitter_lib::ExtraOr<'tree, Attribute<'tree>>>,
  > + 'a {
    self
      .0
      .children_by_field_name("attribute", c)
      .map(|n| <type_sitter_lib::ExtraOr<'tree, Attribute<'tree>> as TryFrom<_>>::try_from(n))
  }

  #[doc = "Get the field `path` which has kind `path` ([Path])"]
  #[allow(dead_code)]
  #[inline]
  pub fn path(&self) -> type_sitter_lib::NodeResult<'tree, Path<'tree>> {
    self . 0 . child_by_field_name ("path") . map (< Path < 'tree > as TryFrom < _ >> :: try_from) . expect ("tree-sitter node missing its required child, there should at least be a MISSING node in its place")
  }
}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Using<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "using" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Using<'tree> {
  const KIND: &'static str = "using";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `visibility`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Visibility<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Visibility<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Visibility<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "visibility" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Visibility<'tree> {
  const KIND: &'static str = "visibility";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `char`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct Char<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> Char<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Char<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "char" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for Char<'tree> {
  const KIND: &'static str = "char";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `hash_bang`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct HashBang<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> HashBang<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for HashBang<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "hash_bang" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for HashBang<'tree> {
  const KIND: &'static str = "hash_bang";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `rest_pattern`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct RestPattern<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> RestPattern<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for RestPattern<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "rest_pattern" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for RestPattern<'tree> {
  const KIND: &'static str = "rest_pattern";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `simple_identifier`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct SimpleIdentifier<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> SimpleIdentifier<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SimpleIdentifier<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "simple_identifier" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for SimpleIdentifier<'tree> {
  const KIND: &'static str = "simple_identifier";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
#[doc = "Typed node `string`\n\nThis node has no children\n"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub struct String<'tree>(tree_sitter::Node<'tree>);
#[automatically_derived]
impl<'tree> String<'tree> {}
#[automatically_derived]
impl<'tree> TryFrom<tree_sitter::Node<'tree>> for String<'tree> {
  type Error = type_sitter_lib::IncorrectKind<'tree>;

  #[inline]
  fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
    if node.kind() == "string" {
      Ok(Self(node))
    } else {
      Err(type_sitter_lib::IncorrectKind {
        node,
        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
      })
    }
  }
}
#[automatically_derived]
impl<'tree> type_sitter_lib::TypedNode<'tree> for String<'tree> {
  const KIND: &'static str = "string";

  #[inline]
  fn node(&self) -> &tree_sitter::Node<'tree> {
    &self.0
  }

  #[inline]
  fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
    &mut self.0
  }

  #[inline]
  fn into_node(self) -> tree_sitter::Node<'tree> {
    self.0
  }

  #[inline]
  unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
    Self(node)
  }
}
pub mod unnamed {
  #[allow(unused_imports)]
  use super::*;
  #[doc = "Typed node `class`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Class<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Class<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Class<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "class" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Class<'tree> {
    const KIND: &'static str = "class";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `data`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Data<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Data<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Data<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "data" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Data<'tree> {
    const KIND: &'static str = "data";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `else`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Else<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Else<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Else<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "else" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Else<'tree> {
    const KIND: &'static str = "else";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `f32`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct F32<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> F32<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for F32<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "f32" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for F32<'tree> {
    const KIND: &'static str = "f32";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `f64`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct F64<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> F64<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for F64<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "f64" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for F64<'tree> {
    const KIND: &'static str = "f64";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `forall`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Forall<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Forall<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Forall<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "forall" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Forall<'tree> {
    const KIND: &'static str = "forall";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `i128`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct I128<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> I128<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I128<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "i128" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for I128<'tree> {
    const KIND: &'static str = "i128";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `i16`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct I16<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> I16<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I16<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "i16" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for I16<'tree> {
    const KIND: &'static str = "i16";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `i64`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct I64<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> I64<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I64<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "i64" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for I64<'tree> {
    const KIND: &'static str = "i64";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `i8`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct I8<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> I8<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for I8<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "i8" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for I8<'tree> {
    const KIND: &'static str = "i8";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `if`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct If<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> If<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for If<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "if" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for If<'tree> {
    const KIND: &'static str = "if";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `instance`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Instance<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Instance<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Instance<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "instance" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Instance<'tree> {
    const KIND: &'static str = "instance";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `internal`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Internal<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Internal<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Internal<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "internal" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Internal<'tree> {
    const KIND: &'static str = "internal";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `is`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Is<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Is<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Is<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "is" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Is<'tree> {
    const KIND: &'static str = "is";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `let`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Let<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Let<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Let<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "let" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Let<'tree> {
    const KIND: &'static str = "let";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `match`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Match<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Match<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Match<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "match" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Match<'tree> {
    const KIND: &'static str = "match";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `n`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct N<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> N<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for N<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "n" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for N<'tree> {
    const KIND: &'static str = "n";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `of`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Of<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Of<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Of<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "of" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Of<'tree> {
    const KIND: &'static str = "of";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `private`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Private<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Private<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Private<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "private" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Private<'tree> {
    const KIND: &'static str = "private";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `public`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Public<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Public<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Public<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "public" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Public<'tree> {
    const KIND: &'static str = "public";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `return`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Return<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Return<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Return<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "return" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Return<'tree> {
    const KIND: &'static str = "return";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `sealed`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Sealed<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Sealed<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Sealed<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "sealed" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Sealed<'tree> {
    const KIND: &'static str = "sealed";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `then`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Then<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Then<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Then<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "then" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Then<'tree> {
    const KIND: &'static str = "then";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `trait`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Trait<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Trait<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Trait<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "trait" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Trait<'tree> {
    const KIND: &'static str = "trait";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `type`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Type<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Type<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Type<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "type" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Type<'tree> {
    const KIND: &'static str = "type";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `u1`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct U1<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> U1<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U1<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "u1" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for U1<'tree> {
    const KIND: &'static str = "u1";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `u128`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct U128<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> U128<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U128<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "u128" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for U128<'tree> {
    const KIND: &'static str = "u128";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `u16`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct U16<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> U16<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U16<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "u16" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for U16<'tree> {
    const KIND: &'static str = "u16";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `u32`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct U32<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> U32<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U32<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "u32" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for U32<'tree> {
    const KIND: &'static str = "u32";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `u64`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct U64<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> U64<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U64<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "u64" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for U64<'tree> {
    const KIND: &'static str = "u64";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `u8`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct U8<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> U8<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for U8<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "u8" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for U8<'tree> {
    const KIND: &'static str = "u8";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `using`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Using<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Using<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Using<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "using" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Using<'tree> {
    const KIND: &'static str = "using";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
}
pub mod symbols {
  #[allow(unused_imports)]
  use super::*;
  #[doc = "Typed node `!`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Not<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Not<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Not<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "!" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Not<'tree> {
    const KIND: &'static str = "!";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `#`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Hash<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Hash<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Hash<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "#" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Hash<'tree> {
    const KIND: &'static str = "#";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `$`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Dollar<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Dollar<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Dollar<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "$" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Dollar<'tree> {
    const KIND: &'static str = "$";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `%`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Mod<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Mod<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Mod<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "%" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Mod<'tree> {
    const KIND: &'static str = "%";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `&`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct And<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> And<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for And<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "&" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for And<'tree> {
    const KIND: &'static str = "&";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `&&`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct AndAnd<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> AndAnd<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for AndAnd<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "&&" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for AndAnd<'tree> {
    const KIND: &'static str = "&&";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `(`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct LParen<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> LParen<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LParen<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "(" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for LParen<'tree> {
    const KIND: &'static str = "(";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `)`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct RParen<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> RParen<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for RParen<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == ")" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for RParen<'tree> {
    const KIND: &'static str = ")";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `*`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Mul<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Mul<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Mul<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "*" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Mul<'tree> {
    const KIND: &'static str = "*";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `+`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Add<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Add<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Add<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "+" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Add<'tree> {
    const KIND: &'static str = "+";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `,`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Comma<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Comma<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Comma<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "," {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Comma<'tree> {
    const KIND: &'static str = ",";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `-`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Sub<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Sub<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Sub<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "-" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Sub<'tree> {
    const KIND: &'static str = "-";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `->`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct SubGt<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> SubGt<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SubGt<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "->" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for SubGt<'tree> {
    const KIND: &'static str = "->";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `.`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Dot<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Dot<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Dot<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "." {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Dot<'tree> {
    const KIND: &'static str = ".";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `/`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Div<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Div<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Div<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "/" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Div<'tree> {
    const KIND: &'static str = "/";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `:`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Colon<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Colon<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Colon<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == ":" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Colon<'tree> {
    const KIND: &'static str = ":";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `;`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Semicolon<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Semicolon<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Semicolon<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == ";" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Semicolon<'tree> {
    const KIND: &'static str = ";";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `<`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Lt<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Lt<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Lt<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "<" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Lt<'tree> {
    const KIND: &'static str = "<";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `<-`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct LtSub<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> LtSub<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LtSub<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "<-" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for LtSub<'tree> {
    const KIND: &'static str = "<-";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `=`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Eq<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Eq<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Eq<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "=" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Eq<'tree> {
    const KIND: &'static str = "=";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `=>`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct EqGt<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> EqGt<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for EqGt<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "=>" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for EqGt<'tree> {
    const KIND: &'static str = "=>";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `>`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Gt<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Gt<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Gt<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == ">" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Gt<'tree> {
    const KIND: &'static str = ">";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `?`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Question<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Question<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Question<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "?" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Question<'tree> {
    const KIND: &'static str = "?";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `@`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct At<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> At<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for At<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "@" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for At<'tree> {
    const KIND: &'static str = "@";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `[`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct LBracket<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> LBracket<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LBracket<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "[" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for LBracket<'tree> {
    const KIND: &'static str = "[";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `]`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct RBracket<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> RBracket<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for RBracket<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "]" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for RBracket<'tree> {
    const KIND: &'static str = "]";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `^`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct BitXor<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> BitXor<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for BitXor<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "^" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for BitXor<'tree> {
    const KIND: &'static str = "^";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node ```\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Backtick<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Backtick<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Backtick<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "`" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Backtick<'tree> {
    const KIND: &'static str = "`";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `{`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct LBrace<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> LBrace<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for LBrace<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "{" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for LBrace<'tree> {
    const KIND: &'static str = "{";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `|`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct Or<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> Or<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Or<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "|" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Or<'tree> {
    const KIND: &'static str = "|";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `||`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct OrOr<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> OrOr<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for OrOr<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "||" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for OrOr<'tree> {
    const KIND: &'static str = "||";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `}`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct RBrace<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> RBrace<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for RBrace<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "}" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for RBrace<'tree> {
    const KIND: &'static str = "}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
  #[doc = "Typed node `~`\n\nThis node has no children\n"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub struct BitNot<'tree>(tree_sitter::Node<'tree>);
  #[automatically_derived]
  impl<'tree> BitNot<'tree> {}
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for BitNot<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      if node.kind() == "~" {
        Ok(Self(node))
      } else {
        Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        })
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for BitNot<'tree> {
    const KIND: &'static str = "~";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      &self.0
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      &mut self.0
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      self.0
    }

    #[inline]
    unsafe fn from_node_unchecked(node: tree_sitter::Node<'tree>) -> Self {
      Self(node)
    }
  }
}
pub mod anon_unions {
  #[allow(unused_imports)]
  use super::*;
  #[doc = "one of `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}`:\n- [AnnExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]\n- [TypeAppExpr]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr<
    'tree,
  > {
    AnnExpr(AnnExpr<'tree>),
    BinaryExpr(BinaryExpr<'tree>),
    ForallExpr(ForallExpr<'tree>),
    LamExpr(LamExpr<'tree>),
    MatchExpr(MatchExpr<'tree>),
    PiExpr(PiExpr<'tree>),
    Primary(Primary<'tree>),
    SigmaExpr(SigmaExpr<'tree>),
    TypeAppExpr(TypeAppExpr<'tree>),
  }
  #[automatically_derived]
  impl<'tree>
    AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr<'tree>
  {
    #[doc = "Returns the node if it is of kind `ann_expr` ([AnnExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn ann_expr(self) -> Option<AnnExpr<'tree>> {
      match self {
        Self::AnnExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `binary_expr` ([BinaryExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn binary_expr(self) -> Option<BinaryExpr<'tree>> {
      match self {
        Self::BinaryExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `forall_expr` ([ForallExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn forall_expr(self) -> Option<ForallExpr<'tree>> {
      match self {
        Self::ForallExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `lam_expr` ([LamExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn lam_expr(self) -> Option<LamExpr<'tree>> {
      match self {
        Self::LamExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `match_expr` ([MatchExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn match_expr(self) -> Option<MatchExpr<'tree>> {
      match self {
        Self::MatchExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `pi_expr` ([PiExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn pi_expr(self) -> Option<PiExpr<'tree>> {
      match self {
        Self::PiExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `primary` ([Primary]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn primary(self) -> Option<Primary<'tree>> {
      match self {
        Self::Primary(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `sigma_expr` ([SigmaExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn sigma_expr(self) -> Option<SigmaExpr<'tree>> {
      match self {
        Self::SigmaExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `type_app_expr` ([TypeAppExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn type_app_expr(self) -> Option<TypeAppExpr<'tree>> {
      match self {
        Self::TypeAppExpr(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "ann_expr" => Ok(unsafe {
          Self::AnnExpr(
            <AnnExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "binary_expr" => Ok(unsafe {
          Self::BinaryExpr(
            <BinaryExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "forall_expr" => Ok(unsafe {
          Self::ForallExpr(
            <ForallExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "lam_expr" => Ok(unsafe {
          Self::LamExpr(
            <LamExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "match_expr" => Ok(unsafe {
          Self::MatchExpr(
            <MatchExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "pi_expr" => Ok(unsafe {
          Self::PiExpr(
            <PiExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "primary" => Ok(unsafe {
          Self::Primary(
            <Primary<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "sigma_expr" => Ok(unsafe {
          Self::SigmaExpr(
            <SigmaExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "type_app_expr" => Ok(unsafe {
          Self::TypeAppExpr(
            <TypeAppExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr<'tree>
  {
    const KIND : & 'static str = "{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | type_app_expr}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node(),
        Self::BinaryExpr(x) => x.node(),
        Self::ForallExpr(x) => x.node(),
        Self::LamExpr(x) => x.node(),
        Self::MatchExpr(x) => x.node(),
        Self::PiExpr(x) => x.node(),
        Self::Primary(x) => x.node(),
        Self::SigmaExpr(x) => x.node(),
        Self::TypeAppExpr(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node_mut(),
        Self::BinaryExpr(x) => x.node_mut(),
        Self::ForallExpr(x) => x.node_mut(),
        Self::LamExpr(x) => x.node_mut(),
        Self::MatchExpr(x) => x.node_mut(),
        Self::PiExpr(x) => x.node_mut(),
        Self::Primary(x) => x.node_mut(),
        Self::SigmaExpr(x) => x.node_mut(),
        Self::TypeAppExpr(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.into_node(),
        Self::BinaryExpr(x) => x.into_node(),
        Self::ForallExpr(x) => x.into_node(),
        Self::LamExpr(x) => x.into_node(),
        Self::MatchExpr(x) => x.into_node(),
        Self::PiExpr(x) => x.into_node(),
        Self::Primary(x) => x.into_node(),
        Self::SigmaExpr(x) => x.into_node(),
        Self::TypeAppExpr(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree> {
    AnnExpr(AnnExpr<'tree>),
    AppExpr(AppExpr<'tree>),
    BinaryExpr(BinaryExpr<'tree>),
    ForallExpr(ForallExpr<'tree>),
    LamExpr(LamExpr<'tree>),
    MatchExpr(MatchExpr<'tree>),
    PiExpr(PiExpr<'tree>),
    Primary(Primary<'tree>),
    SigmaExpr(SigmaExpr<'tree>),
  }
  #[automatically_derived]
  impl<'tree>
    AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>
  {
    #[doc = "Returns the node if it is of kind `ann_expr` ([AnnExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn ann_expr(self) -> Option<AnnExpr<'tree>> {
      match self {
        Self::AnnExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `app_expr` ([AppExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn app_expr(self) -> Option<AppExpr<'tree>> {
      match self {
        Self::AppExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `binary_expr` ([BinaryExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn binary_expr(self) -> Option<BinaryExpr<'tree>> {
      match self {
        Self::BinaryExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `forall_expr` ([ForallExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn forall_expr(self) -> Option<ForallExpr<'tree>> {
      match self {
        Self::ForallExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `lam_expr` ([LamExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn lam_expr(self) -> Option<LamExpr<'tree>> {
      match self {
        Self::LamExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `match_expr` ([MatchExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn match_expr(self) -> Option<MatchExpr<'tree>> {
      match self {
        Self::MatchExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `pi_expr` ([PiExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn pi_expr(self) -> Option<PiExpr<'tree>> {
      match self {
        Self::PiExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `primary` ([Primary]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn primary(self) -> Option<Primary<'tree>> {
      match self {
        Self::Primary(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `sigma_expr` ([SigmaExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn sigma_expr(self) -> Option<SigmaExpr<'tree>> {
      match self {
        Self::SigmaExpr(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "ann_expr" => Ok(unsafe {
          Self::AnnExpr(
            <AnnExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "app_expr" => Ok(unsafe {
          Self::AppExpr(
            <AppExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "binary_expr" => Ok(unsafe {
          Self::BinaryExpr(
            <BinaryExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "forall_expr" => Ok(unsafe {
          Self::ForallExpr(
            <ForallExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "lam_expr" => Ok(unsafe {
          Self::LamExpr(
            <LamExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "match_expr" => Ok(unsafe {
          Self::MatchExpr(
            <MatchExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "pi_expr" => Ok(unsafe {
          Self::PiExpr(
            <PiExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "primary" => Ok(unsafe {
          Self::Primary(
            <Primary<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "sigma_expr" => Ok(unsafe {
          Self::SigmaExpr(
            <SigmaExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>
  {
    const KIND : & 'static str = "{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node(),
        Self::AppExpr(x) => x.node(),
        Self::BinaryExpr(x) => x.node(),
        Self::ForallExpr(x) => x.node(),
        Self::LamExpr(x) => x.node(),
        Self::MatchExpr(x) => x.node(),
        Self::PiExpr(x) => x.node(),
        Self::Primary(x) => x.node(),
        Self::SigmaExpr(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node_mut(),
        Self::AppExpr(x) => x.node_mut(),
        Self::BinaryExpr(x) => x.node_mut(),
        Self::ForallExpr(x) => x.node_mut(),
        Self::LamExpr(x) => x.node_mut(),
        Self::MatchExpr(x) => x.node_mut(),
        Self::PiExpr(x) => x.node_mut(),
        Self::Primary(x) => x.node_mut(),
        Self::SigmaExpr(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.into_node(),
        Self::AppExpr(x) => x.into_node(),
        Self::BinaryExpr(x) => x.into_node(),
        Self::ForallExpr(x) => x.into_node(),
        Self::LamExpr(x) => x.into_node(),
        Self::MatchExpr(x) => x.into_node(),
        Self::PiExpr(x) => x.into_node(),
        Self::Primary(x) => x.into_node(),
        Self::SigmaExpr(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{block | primary}`:\n- [Block]\n- [Primary]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Block_Primary<'tree> {
    Block(Block<'tree>),
    Primary(Primary<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Block_Primary<'tree> {
    #[doc = "Returns the node if it is of kind `block` ([Block]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn block(self) -> Option<Block<'tree>> {
      match self {
        Self::Block(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `primary` ([Primary]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn primary(self) -> Option<Primary<'tree>> {
      match self {
        Self::Primary(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Block_Primary<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "block" => Ok(unsafe {
          Self::Block(
            <Block<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "primary" => Ok(unsafe {
          Self::Primary(
            <Primary<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Block_Primary<'tree> {
    const KIND: &'static str = "{block | primary}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Block(x) => x.node(),
        Self::Primary(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Block(x) => x.node_mut(),
        Self::Primary(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Block(x) => x.into_node(),
        Self::Primary(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{cons_pattern | group_pattern | literal | rest_pattern}`:\n- [ConsPattern]\n- [GroupPattern]\n- [Literal]\n- [RestPattern]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum ConsPattern_GroupPattern_Literal_RestPattern<'tree> {
    ConsPattern(ConsPattern<'tree>),
    GroupPattern(GroupPattern<'tree>),
    Literal(Literal<'tree>),
    RestPattern(RestPattern<'tree>),
  }
  #[automatically_derived]
  impl<'tree> ConsPattern_GroupPattern_Literal_RestPattern<'tree> {
    #[doc = "Returns the node if it is of kind `cons_pattern` ([ConsPattern]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn cons_pattern(self) -> Option<ConsPattern<'tree>> {
      match self {
        Self::ConsPattern(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `group_pattern` ([GroupPattern]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn group_pattern(self) -> Option<GroupPattern<'tree>> {
      match self {
        Self::GroupPattern(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `literal` ([Literal]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn literal(self) -> Option<Literal<'tree>> {
      match self {
        Self::Literal(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `rest_pattern` ([RestPattern]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn rest_pattern(self) -> Option<RestPattern<'tree>> {
      match self {
        Self::RestPattern(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for ConsPattern_GroupPattern_Literal_RestPattern<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "cons_pattern" => Ok(unsafe {
          Self::ConsPattern(
            <ConsPattern<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "group_pattern" => Ok(unsafe {
          Self::GroupPattern(
            <GroupPattern<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "literal" => Ok(unsafe {
          Self::Literal(
            <Literal<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "rest_pattern" => Ok(unsafe {
          Self::RestPattern(
            <RestPattern<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for ConsPattern_GroupPattern_Literal_RestPattern<'tree>
  {
    const KIND: &'static str = "{cons_pattern | group_pattern | literal | rest_pattern}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::ConsPattern(x) => x.node(),
        Self::GroupPattern(x) => x.node(),
        Self::Literal(x) => x.node(),
        Self::RestPattern(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::ConsPattern(x) => x.node_mut(),
        Self::GroupPattern(x) => x.node_mut(),
        Self::Literal(x) => x.node_mut(),
        Self::RestPattern(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::ConsPattern(x) => x.into_node(),
        Self::GroupPattern(x) => x.into_node(),
        Self::Literal(x) => x.into_node(),
        Self::RestPattern(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | path}`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]\n- [Path]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path<
    'tree,
  > {
    AnnExpr(AnnExpr<'tree>),
    AppExpr(AppExpr<'tree>),
    BinaryExpr(BinaryExpr<'tree>),
    ForallExpr(ForallExpr<'tree>),
    LamExpr(LamExpr<'tree>),
    MatchExpr(MatchExpr<'tree>),
    PiExpr(PiExpr<'tree>),
    Primary(Primary<'tree>),
    SigmaExpr(SigmaExpr<'tree>),
    Path(Path<'tree>),
  }
  #[automatically_derived]
  impl<'tree>
    AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path<'tree>
  {
    #[doc = "Returns the node if it is of kind `ann_expr` ([AnnExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn ann_expr(self) -> Option<AnnExpr<'tree>> {
      match self {
        Self::AnnExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `app_expr` ([AppExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn app_expr(self) -> Option<AppExpr<'tree>> {
      match self {
        Self::AppExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `binary_expr` ([BinaryExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn binary_expr(self) -> Option<BinaryExpr<'tree>> {
      match self {
        Self::BinaryExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `forall_expr` ([ForallExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn forall_expr(self) -> Option<ForallExpr<'tree>> {
      match self {
        Self::ForallExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `lam_expr` ([LamExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn lam_expr(self) -> Option<LamExpr<'tree>> {
      match self {
        Self::LamExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `match_expr` ([MatchExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn match_expr(self) -> Option<MatchExpr<'tree>> {
      match self {
        Self::MatchExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `pi_expr` ([PiExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn pi_expr(self) -> Option<PiExpr<'tree>> {
      match self {
        Self::PiExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `primary` ([Primary]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn primary(self) -> Option<Primary<'tree>> {
      match self {
        Self::Primary(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `sigma_expr` ([SigmaExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn sigma_expr(self) -> Option<SigmaExpr<'tree>> {
      match self {
        Self::SigmaExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `path` ([Path]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn path(self) -> Option<Path<'tree>> {
      match self {
        Self::Path(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "ann_expr" => Ok(unsafe {
          Self::AnnExpr(
            <AnnExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "app_expr" => Ok(unsafe {
          Self::AppExpr(
            <AppExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "binary_expr" => Ok(unsafe {
          Self::BinaryExpr(
            <BinaryExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "forall_expr" => Ok(unsafe {
          Self::ForallExpr(
            <ForallExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "lam_expr" => Ok(unsafe {
          Self::LamExpr(
            <LamExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "match_expr" => Ok(unsafe {
          Self::MatchExpr(
            <MatchExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "pi_expr" => Ok(unsafe {
          Self::PiExpr(
            <PiExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "primary" => Ok(unsafe {
          Self::Primary(
            <Primary<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "sigma_expr" => Ok(unsafe {
          Self::SigmaExpr(
            <SigmaExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "path" => Ok(unsafe {
          Self::Path(<Path<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_Path<'tree>
  {
    const KIND : & 'static str = "{ann_expr | app_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr | path}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node(),
        Self::AppExpr(x) => x.node(),
        Self::BinaryExpr(x) => x.node(),
        Self::ForallExpr(x) => x.node(),
        Self::LamExpr(x) => x.node(),
        Self::MatchExpr(x) => x.node(),
        Self::PiExpr(x) => x.node(),
        Self::Primary(x) => x.node(),
        Self::SigmaExpr(x) => x.node(),
        Self::Path(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node_mut(),
        Self::AppExpr(x) => x.node_mut(),
        Self::BinaryExpr(x) => x.node_mut(),
        Self::ForallExpr(x) => x.node_mut(),
        Self::LamExpr(x) => x.node_mut(),
        Self::MatchExpr(x) => x.node_mut(),
        Self::PiExpr(x) => x.node_mut(),
        Self::Primary(x) => x.node_mut(),
        Self::SigmaExpr(x) => x.node_mut(),
        Self::Path(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.into_node(),
        Self::AppExpr(x) => x.into_node(),
        Self::BinaryExpr(x) => x.into_node(),
        Self::ForallExpr(x) => x.into_node(),
        Self::LamExpr(x) => x.into_node(),
        Self::MatchExpr(x) => x.into_node(),
        Self::PiExpr(x) => x.into_node(),
        Self::Primary(x) => x.into_node(),
        Self::SigmaExpr(x) => x.into_node(),
        Self::Path(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{ask_stmt | expr_stmt | if_stmt | let_stmt}`:\n- [AskStmt]\n- [ExprStmt]\n- [IfStmt]\n- [LetStmt]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum AskStmt_ExprStmt_IfStmt_LetStmt<'tree> {
    AskStmt(AskStmt<'tree>),
    ExprStmt(ExprStmt<'tree>),
    IfStmt(IfStmt<'tree>),
    LetStmt(LetStmt<'tree>),
  }
  #[automatically_derived]
  impl<'tree> AskStmt_ExprStmt_IfStmt_LetStmt<'tree> {
    #[doc = "Returns the node if it is of kind `ask_stmt` ([AskStmt]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn ask_stmt(self) -> Option<AskStmt<'tree>> {
      match self {
        Self::AskStmt(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `expr_stmt` ([ExprStmt]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn expr_stmt(self) -> Option<ExprStmt<'tree>> {
      match self {
        Self::ExprStmt(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `if_stmt` ([IfStmt]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn if_stmt(self) -> Option<IfStmt<'tree>> {
      match self {
        Self::IfStmt(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `let_stmt` ([LetStmt]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn let_stmt(self) -> Option<LetStmt<'tree>> {
      match self {
        Self::LetStmt(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for AskStmt_ExprStmt_IfStmt_LetStmt<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "ask_stmt" => Ok(unsafe {
          Self::AskStmt(
            <AskStmt<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "expr_stmt" => Ok(unsafe {
          Self::ExprStmt(
            <ExprStmt<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "if_stmt" => Ok(unsafe {
          Self::IfStmt(
            <IfStmt<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "let_stmt" => Ok(unsafe {
          Self::LetStmt(
            <LetStmt<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for AskStmt_ExprStmt_IfStmt_LetStmt<'tree> {
    const KIND: &'static str = "{ask_stmt | expr_stmt | if_stmt | let_stmt}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::AskStmt(x) => x.node(),
        Self::ExprStmt(x) => x.node(),
        Self::IfStmt(x) => x.node(),
        Self::LetStmt(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::AskStmt(x) => x.node_mut(),
        Self::ExprStmt(x) => x.node_mut(),
        Self::IfStmt(x) => x.node_mut(),
        Self::LetStmt(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::AskStmt(x) => x.into_node(),
        Self::ExprStmt(x) => x.into_node(),
        Self::IfStmt(x) => x.into_node(),
        Self::LetStmt(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{explicit_arguments | implicit_arguments}`:\n- [ExplicitArguments]\n- [ImplicitArguments]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum ExplicitArguments_ImplicitArguments<'tree> {
    ExplicitArguments(ExplicitArguments<'tree>),
    ImplicitArguments(ImplicitArguments<'tree>),
  }
  #[automatically_derived]
  impl<'tree> ExplicitArguments_ImplicitArguments<'tree> {
    #[doc = "Returns the node if it is of kind `explicit_arguments` ([ExplicitArguments]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn explicit_arguments(self) -> Option<ExplicitArguments<'tree>> {
      match self {
        Self::ExplicitArguments(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `implicit_arguments` ([ImplicitArguments]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn implicit_arguments(self) -> Option<ImplicitArguments<'tree>> {
      match self {
        Self::ImplicitArguments(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ExplicitArguments_ImplicitArguments<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "explicit_arguments" => Ok(unsafe {
          Self::ExplicitArguments(<ExplicitArguments<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "implicit_arguments" => Ok(unsafe {
          Self::ImplicitArguments(<ImplicitArguments<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for ExplicitArguments_ImplicitArguments<'tree> {
    const KIND: &'static str = "{explicit_arguments | implicit_arguments}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::ExplicitArguments(x) => x.node(),
        Self::ImplicitArguments(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::ExplicitArguments(x) => x.node_mut(),
        Self::ImplicitArguments(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::ExplicitArguments(x) => x.into_node(),
        Self::ImplicitArguments(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{signature | { | }}`:\n- [Signature]\n- [symbols::LBrace]\n- [symbols::RBrace]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Signature_LBrace_RBrace<'tree> {
    Signature(Signature<'tree>),
    LBrace(symbols::LBrace<'tree>),
    RBrace(symbols::RBrace<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Signature_LBrace_RBrace<'tree> {
    #[doc = "Returns the node if it is of kind `signature` ([Signature]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn signature(self) -> Option<Signature<'tree>> {
      match self {
        Self::Signature(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `{` ([symbols::LBrace]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn l_brace(self) -> Option<symbols::LBrace<'tree>> {
      match self {
        Self::LBrace(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `}` ([symbols::RBrace]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn r_brace(self) -> Option<symbols::RBrace<'tree>> {
      match self {
        Self::RBrace(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Signature_LBrace_RBrace<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "signature" => Ok(unsafe {
          Self::Signature(
            <Signature<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "{" => Ok(unsafe {
          Self::LBrace(<symbols::LBrace<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "}" => Ok(unsafe {
          Self::RBrace(<symbols::RBrace<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Signature_LBrace_RBrace<'tree> {
    const KIND: &'static str = "{signature | { | }}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Signature(x) => x.node(),
        Self::LBrace(x) => x.node(),
        Self::RBrace(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Signature(x) => x.node_mut(),
        Self::LBrace(x) => x.node_mut(),
        Self::RBrace(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Signature(x) => x.into_node(),
        Self::LBrace(x) => x.into_node(),
        Self::RBrace(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{, | function_constructor | signature_constructor}`:\n- [symbols::Comma]\n- [FunctionConstructor]\n- [SignatureConstructor]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Comma_FunctionConstructor_SignatureConstructor<'tree> {
    Comma(symbols::Comma<'tree>),
    FunctionConstructor(FunctionConstructor<'tree>),
    SignatureConstructor(SignatureConstructor<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Comma_FunctionConstructor_SignatureConstructor<'tree> {
    #[doc = "Returns the node if it is of kind `,` ([symbols::Comma]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn comma(self) -> Option<symbols::Comma<'tree>> {
      match self {
        Self::Comma(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `function_constructor` ([FunctionConstructor]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn function_constructor(self) -> Option<FunctionConstructor<'tree>> {
      match self {
        Self::FunctionConstructor(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `signature_constructor` ([SignatureConstructor]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn signature_constructor(self) -> Option<SignatureConstructor<'tree>> {
      match self {
        Self::SignatureConstructor(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for Comma_FunctionConstructor_SignatureConstructor<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "," => Ok(unsafe {
          Self::Comma(<symbols::Comma<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "function_constructor" => Ok(unsafe {
          Self::FunctionConstructor(<FunctionConstructor<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "signature_constructor" => {
          Ok(unsafe {
            Self :: SignatureConstructor (< SignatureConstructor < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
          })
        }
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for Comma_FunctionConstructor_SignatureConstructor<'tree>
  {
    const KIND: &'static str = "{, | function_constructor | signature_constructor}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node(),
        Self::FunctionConstructor(x) => x.node(),
        Self::SignatureConstructor(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node_mut(),
        Self::FunctionConstructor(x) => x.node_mut(),
        Self::SignatureConstructor(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.into_node(),
        Self::FunctionConstructor(x) => x.into_node(),
        Self::SignatureConstructor(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{, | ; | function_constructor | signature | signature_constructor | { | }}`:\n- [symbols::Comma]\n- [symbols::Semicolon]\n- [FunctionConstructor]\n- [Signature]\n- [SignatureConstructor]\n- [symbols::LBrace]\n- [symbols::RBrace]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace<'tree> {
    Comma(symbols::Comma<'tree>),
    Semicolon(symbols::Semicolon<'tree>),
    FunctionConstructor(FunctionConstructor<'tree>),
    Signature(Signature<'tree>),
    SignatureConstructor(SignatureConstructor<'tree>),
    LBrace(symbols::LBrace<'tree>),
    RBrace(symbols::RBrace<'tree>),
  }
  #[automatically_derived]
  impl<'tree>
    Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace<'tree>
  {
    #[doc = "Returns the node if it is of kind `,` ([symbols::Comma]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn comma(self) -> Option<symbols::Comma<'tree>> {
      match self {
        Self::Comma(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `;` ([symbols::Semicolon]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn semicolon(self) -> Option<symbols::Semicolon<'tree>> {
      match self {
        Self::Semicolon(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `function_constructor` ([FunctionConstructor]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn function_constructor(self) -> Option<FunctionConstructor<'tree>> {
      match self {
        Self::FunctionConstructor(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `signature` ([Signature]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn signature(self) -> Option<Signature<'tree>> {
      match self {
        Self::Signature(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `signature_constructor` ([SignatureConstructor]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn signature_constructor(self) -> Option<SignatureConstructor<'tree>> {
      match self {
        Self::SignatureConstructor(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `{` ([symbols::LBrace]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn l_brace(self) -> Option<symbols::LBrace<'tree>> {
      match self {
        Self::LBrace(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `}` ([symbols::RBrace]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn r_brace(self) -> Option<symbols::RBrace<'tree>> {
      match self {
        Self::RBrace(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "," => Ok(unsafe {
          Self::Comma(<symbols::Comma<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        ";" => Ok(unsafe {
          Self::Semicolon(<symbols::Semicolon<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "function_constructor" => Ok(unsafe {
          Self::FunctionConstructor(<FunctionConstructor<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "signature" => Ok(unsafe {
          Self::Signature(
            <Signature<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "signature_constructor" => {
          Ok(unsafe {
            Self :: SignatureConstructor (< SignatureConstructor < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
          })
        }
        "{" => Ok(unsafe {
          Self::LBrace(<symbols::LBrace<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "}" => Ok(unsafe {
          Self::RBrace(<symbols::RBrace<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for Comma_Semicolon_FunctionConstructor_Signature_SignatureConstructor_LBrace_RBrace<'tree>
  {
    const KIND: &'static str =
      "{, | ; | function_constructor | signature | signature_constructor | { | }}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node(),
        Self::Semicolon(x) => x.node(),
        Self::FunctionConstructor(x) => x.node(),
        Self::Signature(x) => x.node(),
        Self::SignatureConstructor(x) => x.node(),
        Self::LBrace(x) => x.node(),
        Self::RBrace(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node_mut(),
        Self::Semicolon(x) => x.node_mut(),
        Self::FunctionConstructor(x) => x.node_mut(),
        Self::Signature(x) => x.node_mut(),
        Self::SignatureConstructor(x) => x.node_mut(),
        Self::LBrace(x) => x.node_mut(),
        Self::RBrace(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.into_node(),
        Self::Semicolon(x) => x.into_node(),
        Self::FunctionConstructor(x) => x.into_node(),
        Self::Signature(x) => x.into_node(),
        Self::SignatureConstructor(x) => x.into_node(),
        Self::LBrace(x) => x.into_node(),
        Self::RBrace(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{forall_parameter | parameter}`:\n- [ForallParameter]\n- [Parameter]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum ForallParameter_Parameter<'tree> {
    ForallParameter(ForallParameter<'tree>),
    Parameter(Parameter<'tree>),
  }
  #[automatically_derived]
  impl<'tree> ForallParameter_Parameter<'tree> {
    #[doc = "Returns the node if it is of kind `forall_parameter` ([ForallParameter]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn forall_parameter(self) -> Option<ForallParameter<'tree>> {
      match self {
        Self::ForallParameter(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `parameter` ([Parameter]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn parameter(self) -> Option<Parameter<'tree>> {
      match self {
        Self::Parameter(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for ForallParameter_Parameter<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "forall_parameter" => Ok(unsafe {
          Self::ForallParameter(<ForallParameter<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "parameter" => Ok(unsafe {
          Self::Parameter(
            <Parameter<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for ForallParameter_Parameter<'tree> {
    const KIND: &'static str = "{forall_parameter | parameter}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::ForallParameter(x) => x.node(),
        Self::Parameter(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::ForallParameter(x) => x.node_mut(),
        Self::Parameter(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::ForallParameter(x) => x.into_node(),
        Self::Parameter(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{binary | hex | octal}`:\n- [Binary]\n- [Hex]\n- [Octal]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Binary_Hex_Octal<'tree> {
    Binary(Binary<'tree>),
    Hex(Hex<'tree>),
    Octal(Octal<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Binary_Hex_Octal<'tree> {
    #[doc = "Returns the node if it is of kind `binary` ([Binary]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn binary(self) -> Option<Binary<'tree>> {
      match self {
        Self::Binary(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `hex` ([Hex]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn hex(self) -> Option<Hex<'tree>> {
      match self {
        Self::Hex(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `octal` ([Octal]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn octal(self) -> Option<Octal<'tree>> {
      match self {
        Self::Octal(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Binary_Hex_Octal<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "binary" => Ok(unsafe {
          Self::Binary(
            <Binary<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "hex" => Ok(unsafe {
          Self::Hex(<Hex<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "octal" => Ok(unsafe {
          Self::Octal(
            <Octal<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Binary_Hex_Octal<'tree> {
    const KIND: &'static str = "{binary | hex | octal}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Binary(x) => x.node(),
        Self::Hex(x) => x.node(),
        Self::Octal(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Binary(x) => x.node_mut(),
        Self::Hex(x) => x.node_mut(),
        Self::Octal(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Binary(x) => x.into_node(),
        Self::Hex(x) => x.into_node(),
        Self::Octal(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{simple_identifier | symbol_identifier}`:\n- [SimpleIdentifier]\n- [SymbolIdentifier]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum SimpleIdentifier_SymbolIdentifier<'tree> {
    SimpleIdentifier(SimpleIdentifier<'tree>),
    SymbolIdentifier(SymbolIdentifier<'tree>),
  }
  #[automatically_derived]
  impl<'tree> SimpleIdentifier_SymbolIdentifier<'tree> {
    #[doc = "Returns the node if it is of kind `simple_identifier` ([SimpleIdentifier]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn simple_identifier(self) -> Option<SimpleIdentifier<'tree>> {
      match self {
        Self::SimpleIdentifier(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `symbol_identifier` ([SymbolIdentifier]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn symbol_identifier(self) -> Option<SymbolIdentifier<'tree>> {
      match self {
        Self::SymbolIdentifier(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for SimpleIdentifier_SymbolIdentifier<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "simple_identifier" => Ok(unsafe {
          Self::SimpleIdentifier(<SimpleIdentifier<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "symbol_identifier" => Ok(unsafe {
          Self::SymbolIdentifier(<SymbolIdentifier<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for SimpleIdentifier_SymbolIdentifier<'tree> {
    const KIND: &'static str = "{simple_identifier | symbol_identifier}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::SimpleIdentifier(x) => x.node(),
        Self::SymbolIdentifier(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::SimpleIdentifier(x) => x.node_mut(),
        Self::SymbolIdentifier(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::SimpleIdentifier(x) => x.into_node(),
        Self::SymbolIdentifier(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{, | forall_parameter | parameter}`:\n- [symbols::Comma]\n- [ForallParameter]\n- [Parameter]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Comma_ForallParameter_Parameter<'tree> {
    Comma(symbols::Comma<'tree>),
    ForallParameter(ForallParameter<'tree>),
    Parameter(Parameter<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Comma_ForallParameter_Parameter<'tree> {
    #[doc = "Returns the node if it is of kind `,` ([symbols::Comma]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn comma(self) -> Option<symbols::Comma<'tree>> {
      match self {
        Self::Comma(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `forall_parameter` ([ForallParameter]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn forall_parameter(self) -> Option<ForallParameter<'tree>> {
      match self {
        Self::ForallParameter(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `parameter` ([Parameter]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn parameter(self) -> Option<Parameter<'tree>> {
      match self {
        Self::Parameter(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>> for Comma_ForallParameter_Parameter<'tree> {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "," => Ok(unsafe {
          Self::Comma(<symbols::Comma<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "forall_parameter" => Ok(unsafe {
          Self::ForallParameter(<ForallParameter<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "parameter" => Ok(unsafe {
          Self::Parameter(
            <Parameter<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree> for Comma_ForallParameter_Parameter<'tree> {
    const KIND: &'static str = "{, | forall_parameter | parameter}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node(),
        Self::ForallParameter(x) => x.node(),
        Self::Parameter(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node_mut(),
        Self::ForallParameter(x) => x.node_mut(),
        Self::Parameter(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.into_node(),
        Self::ForallParameter(x) => x.into_node(),
        Self::Parameter(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{char | f32 | f64 | i128 | i16 | i64 | i8 | nat | string | u1 | u128 | u16 | u32 | u64 | u8}`:\n- [Char]\n- [F32]\n- [F64]\n- [I128]\n- [I16]\n- [I64]\n- [I8]\n- [Nat]\n- [String]\n- [U1]\n- [U128]\n- [U16]\n- [U32]\n- [U64]\n- [U8]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8<'tree> {
    Char(Char<'tree>),
    F32(F32<'tree>),
    F64(F64<'tree>),
    I128(I128<'tree>),
    I16(I16<'tree>),
    I64(I64<'tree>),
    I8(I8<'tree>),
    Nat(Nat<'tree>),
    String(String<'tree>),
    U1(U1<'tree>),
    U128(U128<'tree>),
    U16(U16<'tree>),
    U32(U32<'tree>),
    U64(U64<'tree>),
    U8(U8<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8<'tree> {
    #[doc = "Returns the node if it is of kind `char` ([Char]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn char(self) -> Option<Char<'tree>> {
      match self {
        Self::Char(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `f32` ([F32]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn f_32(self) -> Option<F32<'tree>> {
      match self {
        Self::F32(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `f64` ([F64]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn f_64(self) -> Option<F64<'tree>> {
      match self {
        Self::F64(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `i128` ([I128]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn i_128(self) -> Option<I128<'tree>> {
      match self {
        Self::I128(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `i16` ([I16]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn i_16(self) -> Option<I16<'tree>> {
      match self {
        Self::I16(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `i64` ([I64]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn i_64(self) -> Option<I64<'tree>> {
      match self {
        Self::I64(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `i8` ([I8]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn i_8(self) -> Option<I8<'tree>> {
      match self {
        Self::I8(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `nat` ([Nat]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn nat(self) -> Option<Nat<'tree>> {
      match self {
        Self::Nat(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `string` ([String]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn string(self) -> Option<String<'tree>> {
      match self {
        Self::String(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `u1` ([U1]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn u_1(self) -> Option<U1<'tree>> {
      match self {
        Self::U1(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `u128` ([U128]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn u_128(self) -> Option<U128<'tree>> {
      match self {
        Self::U128(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `u16` ([U16]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn u_16(self) -> Option<U16<'tree>> {
      match self {
        Self::U16(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `u32` ([U32]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn u_32(self) -> Option<U32<'tree>> {
      match self {
        Self::U32(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `u64` ([U64]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn u_64(self) -> Option<U64<'tree>> {
      match self {
        Self::U64(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `u8` ([U8]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn u_8(self) -> Option<U8<'tree>> {
      match self {
        Self::U8(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "char" => Ok(unsafe {
          Self::Char(<Char<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "f32" => Ok(unsafe {
          Self::F32(<F32<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "f64" => Ok(unsafe {
          Self::F64(<F64<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "i128" => Ok(unsafe {
          Self::I128(<I128<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "i16" => Ok(unsafe {
          Self::I16(<I16<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "i64" => Ok(unsafe {
          Self::I64(<I64<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "i8" => Ok(unsafe {
          Self::I8(<I8<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "nat" => Ok(unsafe {
          Self::Nat(<Nat<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "string" => Ok(unsafe {
          Self::String(
            <String<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "u1" => Ok(unsafe {
          Self::U1(<U1<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "u128" => Ok(unsafe {
          Self::U128(<U128<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "u16" => Ok(unsafe {
          Self::U16(<U16<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "u32" => Ok(unsafe {
          Self::U32(<U32<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "u64" => Ok(unsafe {
          Self::U64(<U64<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "u8" => Ok(unsafe {
          Self::U8(<U8<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8<'tree>
  {
    const KIND : & 'static str = "{char | f32 | f64 | i128 | i16 | i64 | i8 | nat | string | u1 | u128 | u16 | u32 | u64 | u8}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Char(x) => x.node(),
        Self::F32(x) => x.node(),
        Self::F64(x) => x.node(),
        Self::I128(x) => x.node(),
        Self::I16(x) => x.node(),
        Self::I64(x) => x.node(),
        Self::I8(x) => x.node(),
        Self::Nat(x) => x.node(),
        Self::String(x) => x.node(),
        Self::U1(x) => x.node(),
        Self::U128(x) => x.node(),
        Self::U16(x) => x.node(),
        Self::U32(x) => x.node(),
        Self::U64(x) => x.node(),
        Self::U8(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Char(x) => x.node_mut(),
        Self::F32(x) => x.node_mut(),
        Self::F64(x) => x.node_mut(),
        Self::I128(x) => x.node_mut(),
        Self::I16(x) => x.node_mut(),
        Self::I64(x) => x.node_mut(),
        Self::I8(x) => x.node_mut(),
        Self::Nat(x) => x.node_mut(),
        Self::String(x) => x.node_mut(),
        Self::U1(x) => x.node_mut(),
        Self::U128(x) => x.node_mut(),
        Self::U16(x) => x.node_mut(),
        Self::U32(x) => x.node_mut(),
        Self::U64(x) => x.node_mut(),
        Self::U8(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Char(x) => x.into_node(),
        Self::F32(x) => x.into_node(),
        Self::F64(x) => x.into_node(),
        Self::I128(x) => x.into_node(),
        Self::I16(x) => x.into_node(),
        Self::I64(x) => x.into_node(),
        Self::I8(x) => x.into_node(),
        Self::Nat(x) => x.into_node(),
        Self::String(x) => x.into_node(),
        Self::U1(x) => x.into_node(),
        Self::U128(x) => x.into_node(),
        Self::U16(x) => x.into_node(),
        Self::U32(x) => x.into_node(),
        Self::U64(x) => x.into_node(),
        Self::U8(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}`:\n- [AnnExpr]\n- [AppExpr]\n- [BinaryExpr]\n- [Block]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [Primary]\n- [SigmaExpr]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
    'tree,
  > {
    AnnExpr(AnnExpr<'tree>),
    AppExpr(AppExpr<'tree>),
    BinaryExpr(BinaryExpr<'tree>),
    Block(Block<'tree>),
    ForallExpr(ForallExpr<'tree>),
    LamExpr(LamExpr<'tree>),
    MatchExpr(MatchExpr<'tree>),
    PiExpr(PiExpr<'tree>),
    Primary(Primary<'tree>),
    SigmaExpr(SigmaExpr<'tree>),
  }
  #[automatically_derived]
  impl<'tree>
    AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>
  {
    #[doc = "Returns the node if it is of kind `ann_expr` ([AnnExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn ann_expr(self) -> Option<AnnExpr<'tree>> {
      match self {
        Self::AnnExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `app_expr` ([AppExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn app_expr(self) -> Option<AppExpr<'tree>> {
      match self {
        Self::AppExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `binary_expr` ([BinaryExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn binary_expr(self) -> Option<BinaryExpr<'tree>> {
      match self {
        Self::BinaryExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `block` ([Block]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn block(self) -> Option<Block<'tree>> {
      match self {
        Self::Block(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `forall_expr` ([ForallExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn forall_expr(self) -> Option<ForallExpr<'tree>> {
      match self {
        Self::ForallExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `lam_expr` ([LamExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn lam_expr(self) -> Option<LamExpr<'tree>> {
      match self {
        Self::LamExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `match_expr` ([MatchExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn match_expr(self) -> Option<MatchExpr<'tree>> {
      match self {
        Self::MatchExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `pi_expr` ([PiExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn pi_expr(self) -> Option<PiExpr<'tree>> {
      match self {
        Self::PiExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `primary` ([Primary]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn primary(self) -> Option<Primary<'tree>> {
      match self {
        Self::Primary(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `sigma_expr` ([SigmaExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn sigma_expr(self) -> Option<SigmaExpr<'tree>> {
      match self {
        Self::SigmaExpr(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "ann_expr" => Ok(unsafe {
          Self::AnnExpr(
            <AnnExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "app_expr" => Ok(unsafe {
          Self::AppExpr(
            <AppExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "binary_expr" => Ok(unsafe {
          Self::BinaryExpr(
            <BinaryExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "block" => Ok(unsafe {
          Self::Block(
            <Block<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "forall_expr" => Ok(unsafe {
          Self::ForallExpr(
            <ForallExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "lam_expr" => Ok(unsafe {
          Self::LamExpr(
            <LamExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "match_expr" => Ok(unsafe {
          Self::MatchExpr(
            <MatchExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "pi_expr" => Ok(unsafe {
          Self::PiExpr(
            <PiExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "primary" => Ok(unsafe {
          Self::Primary(
            <Primary<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "sigma_expr" => Ok(unsafe {
          Self::SigmaExpr(
            <SigmaExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<
      'tree,
    >
  {
    const KIND : & 'static str = "{ann_expr | app_expr | binary_expr | block | forall_expr | lam_expr | match_expr | pi_expr | primary | sigma_expr}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node(),
        Self::AppExpr(x) => x.node(),
        Self::BinaryExpr(x) => x.node(),
        Self::Block(x) => x.node(),
        Self::ForallExpr(x) => x.node(),
        Self::LamExpr(x) => x.node(),
        Self::MatchExpr(x) => x.node(),
        Self::PiExpr(x) => x.node(),
        Self::Primary(x) => x.node(),
        Self::SigmaExpr(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.node_mut(),
        Self::AppExpr(x) => x.node_mut(),
        Self::BinaryExpr(x) => x.node_mut(),
        Self::Block(x) => x.node_mut(),
        Self::ForallExpr(x) => x.node_mut(),
        Self::LamExpr(x) => x.node_mut(),
        Self::MatchExpr(x) => x.node_mut(),
        Self::PiExpr(x) => x.node_mut(),
        Self::Primary(x) => x.node_mut(),
        Self::SigmaExpr(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::AnnExpr(x) => x.into_node(),
        Self::AppExpr(x) => x.into_node(),
        Self::BinaryExpr(x) => x.into_node(),
        Self::Block(x) => x.into_node(),
        Self::ForallExpr(x) => x.into_node(),
        Self::LamExpr(x) => x.into_node(),
        Self::MatchExpr(x) => x.into_node(),
        Self::PiExpr(x) => x.into_node(),
        Self::Primary(x) => x.into_node(),
        Self::SigmaExpr(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | pi_named_parameter_set | primary | sigma_expr | type_app_expr}`:\n- [AnnExpr]\n- [BinaryExpr]\n- [ForallExpr]\n- [LamExpr]\n- [MatchExpr]\n- [PiExpr]\n- [PiNamedParameterSet]\n- [Primary]\n- [SigmaExpr]\n- [TypeAppExpr]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr<
    'tree,
  > {
    AnnExpr(AnnExpr<'tree>),
    BinaryExpr(BinaryExpr<'tree>),
    ForallExpr(ForallExpr<'tree>),
    LamExpr(LamExpr<'tree>),
    MatchExpr(MatchExpr<'tree>),
    PiExpr(PiExpr<'tree>),
    PiNamedParameterSet(PiNamedParameterSet<'tree>),
    Primary(Primary<'tree>),
    SigmaExpr(SigmaExpr<'tree>),
    TypeAppExpr(TypeAppExpr<'tree>),
  }
  #[automatically_derived]
  impl < 'tree > AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr < 'tree > { # [doc = "Returns the node if it is of kind `ann_expr` ([AnnExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn ann_expr (self) -> Option < AnnExpr < 'tree > > { match self { Self :: AnnExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `binary_expr` ([BinaryExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn binary_expr (self) -> Option < BinaryExpr < 'tree > > { match self { Self :: BinaryExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `forall_expr` ([ForallExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn forall_expr (self) -> Option < ForallExpr < 'tree > > { match self { Self :: ForallExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `lam_expr` ([LamExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn lam_expr (self) -> Option < LamExpr < 'tree > > { match self { Self :: LamExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `match_expr` ([MatchExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn match_expr (self) -> Option < MatchExpr < 'tree > > { match self { Self :: MatchExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `pi_expr` ([PiExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn pi_expr (self) -> Option < PiExpr < 'tree > > { match self { Self :: PiExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `pi_named_parameter_set` ([PiNamedParameterSet]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn pi_named_parameter_set (self) -> Option < PiNamedParameterSet < 'tree > > { match self { Self :: PiNamedParameterSet (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `primary` ([Primary]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn primary (self) -> Option < Primary < 'tree > > { match self { Self :: Primary (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `sigma_expr` ([SigmaExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn sigma_expr (self) -> Option < SigmaExpr < 'tree > > { match self { Self :: SigmaExpr (x) => Some (x) , _ => None , } } # [doc = "Returns the node if it is of kind `type_app_expr` ([TypeAppExpr]), otherwise returns None"] # [inline] # [allow (unused , non_snake_case)] pub fn type_app_expr (self) -> Option < TypeAppExpr < 'tree > > { match self { Self :: TypeAppExpr (x) => Some (x) , _ => None , } } }
  #[automatically_derived]
  impl < 'tree > TryFrom < tree_sitter :: Node < 'tree >> for AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr < 'tree > { type Error = type_sitter_lib :: IncorrectKind < 'tree > ; # [inline] fn try_from (node : tree_sitter :: Node < 'tree >) -> Result < Self , Self :: Error > { match node . kind () { "ann_expr" => Ok (unsafe { Self :: AnnExpr (< AnnExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "binary_expr" => Ok (unsafe { Self :: BinaryExpr (< BinaryExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "forall_expr" => Ok (unsafe { Self :: ForallExpr (< ForallExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "lam_expr" => Ok (unsafe { Self :: LamExpr (< LamExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "match_expr" => Ok (unsafe { Self :: MatchExpr (< MatchExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "pi_expr" => Ok (unsafe { Self :: PiExpr (< PiExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "pi_named_parameter_set" => Ok (unsafe { Self :: PiNamedParameterSet (< PiNamedParameterSet < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "primary" => Ok (unsafe { Self :: Primary (< Primary < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "sigma_expr" => Ok (unsafe { Self :: SigmaExpr (< SigmaExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , "type_app_expr" => Ok (unsafe { Self :: TypeAppExpr (< TypeAppExpr < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node)) }) , _ => Err (type_sitter_lib :: IncorrectKind { node , kind : < Self as type_sitter_lib :: TypedNode < 'tree >> :: KIND , }) } } }
  #[automatically_derived]
  impl < 'tree > type_sitter_lib :: TypedNode < 'tree > for AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr < 'tree > { const KIND : & 'static str = "{ann_expr | binary_expr | forall_expr | lam_expr | match_expr | pi_expr | pi_named_parameter_set | primary | sigma_expr | type_app_expr}" ; # [inline] fn node (& self) -> & tree_sitter :: Node < 'tree > { match self { Self :: AnnExpr (x) => x . node () , Self :: BinaryExpr (x) => x . node () , Self :: ForallExpr (x) => x . node () , Self :: LamExpr (x) => x . node () , Self :: MatchExpr (x) => x . node () , Self :: PiExpr (x) => x . node () , Self :: PiNamedParameterSet (x) => x . node () , Self :: Primary (x) => x . node () , Self :: SigmaExpr (x) => x . node () , Self :: TypeAppExpr (x) => x . node () , } } # [inline] fn node_mut (& mut self) -> & mut tree_sitter :: Node < 'tree > { match self { Self :: AnnExpr (x) => x . node_mut () , Self :: BinaryExpr (x) => x . node_mut () , Self :: ForallExpr (x) => x . node_mut () , Self :: LamExpr (x) => x . node_mut () , Self :: MatchExpr (x) => x . node_mut () , Self :: PiExpr (x) => x . node_mut () , Self :: PiNamedParameterSet (x) => x . node_mut () , Self :: Primary (x) => x . node_mut () , Self :: SigmaExpr (x) => x . node_mut () , Self :: TypeAppExpr (x) => x . node_mut () , } } # [inline] fn into_node (self) -> tree_sitter :: Node < 'tree > { match self { Self :: AnnExpr (x) => x . into_node () , Self :: BinaryExpr (x) => x . into_node () , Self :: ForallExpr (x) => x . into_node () , Self :: LamExpr (x) => x . into_node () , Self :: MatchExpr (x) => x . into_node () , Self :: PiExpr (x) => x . into_node () , Self :: PiNamedParameterSet (x) => x . into_node () , Self :: Primary (x) => x . into_node () , Self :: SigmaExpr (x) => x . into_node () , Self :: TypeAppExpr (x) => x . into_node () , } } }
  #[doc = "one of `{array_expr | free_variable | if_expr | literal | match_expr | path | return_expr | tuple_expr}`:\n- [ArrayExpr]\n- [FreeVariable]\n- [IfExpr]\n- [Literal]\n- [MatchExpr]\n- [Path]\n- [ReturnExpr]\n- [TupleExpr]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum ArrayExpr_FreeVariable_IfExpr_Literal_MatchExpr_Path_ReturnExpr_TupleExpr<'tree> {
    ArrayExpr(ArrayExpr<'tree>),
    FreeVariable(FreeVariable<'tree>),
    IfExpr(IfExpr<'tree>),
    Literal(Literal<'tree>),
    MatchExpr(MatchExpr<'tree>),
    Path(Path<'tree>),
    ReturnExpr(ReturnExpr<'tree>),
    TupleExpr(TupleExpr<'tree>),
  }
  #[automatically_derived]
  impl<'tree> ArrayExpr_FreeVariable_IfExpr_Literal_MatchExpr_Path_ReturnExpr_TupleExpr<'tree> {
    #[doc = "Returns the node if it is of kind `array_expr` ([ArrayExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn array_expr(self) -> Option<ArrayExpr<'tree>> {
      match self {
        Self::ArrayExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `free_variable` ([FreeVariable]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn free_variable(self) -> Option<FreeVariable<'tree>> {
      match self {
        Self::FreeVariable(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `if_expr` ([IfExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn if_expr(self) -> Option<IfExpr<'tree>> {
      match self {
        Self::IfExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `literal` ([Literal]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn literal(self) -> Option<Literal<'tree>> {
      match self {
        Self::Literal(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `match_expr` ([MatchExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn match_expr(self) -> Option<MatchExpr<'tree>> {
      match self {
        Self::MatchExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `path` ([Path]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn path(self) -> Option<Path<'tree>> {
      match self {
        Self::Path(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `return_expr` ([ReturnExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn return_expr(self) -> Option<ReturnExpr<'tree>> {
      match self {
        Self::ReturnExpr(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `tuple_expr` ([TupleExpr]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn tuple_expr(self) -> Option<TupleExpr<'tree>> {
      match self {
        Self::TupleExpr(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for ArrayExpr_FreeVariable_IfExpr_Literal_MatchExpr_Path_ReturnExpr_TupleExpr<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "array_expr" => Ok(unsafe {
          Self::ArrayExpr(
            <ArrayExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "free_variable" => Ok(unsafe {
          Self::FreeVariable(
            <FreeVariable<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "if_expr" => Ok(unsafe {
          Self::IfExpr(
            <IfExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "literal" => Ok(unsafe {
          Self::Literal(
            <Literal<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "match_expr" => Ok(unsafe {
          Self::MatchExpr(
            <MatchExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "path" => Ok(unsafe {
          Self::Path(<Path<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node))
        }),
        "return_expr" => Ok(unsafe {
          Self::ReturnExpr(
            <ReturnExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "tuple_expr" => Ok(unsafe {
          Self::TupleExpr(
            <TupleExpr<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for ArrayExpr_FreeVariable_IfExpr_Literal_MatchExpr_Path_ReturnExpr_TupleExpr<'tree>
  {
    const KIND : & 'static str = "{array_expr | free_variable | if_expr | literal | match_expr | path | return_expr | tuple_expr}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::ArrayExpr(x) => x.node(),
        Self::FreeVariable(x) => x.node(),
        Self::IfExpr(x) => x.node(),
        Self::Literal(x) => x.node(),
        Self::MatchExpr(x) => x.node(),
        Self::Path(x) => x.node(),
        Self::ReturnExpr(x) => x.node(),
        Self::TupleExpr(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::ArrayExpr(x) => x.node_mut(),
        Self::FreeVariable(x) => x.node_mut(),
        Self::IfExpr(x) => x.node_mut(),
        Self::Literal(x) => x.node_mut(),
        Self::MatchExpr(x) => x.node_mut(),
        Self::Path(x) => x.node_mut(),
        Self::ReturnExpr(x) => x.node_mut(),
        Self::TupleExpr(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::ArrayExpr(x) => x.into_node(),
        Self::FreeVariable(x) => x.into_node(),
        Self::IfExpr(x) => x.into_node(),
        Self::Literal(x) => x.into_node(),
        Self::MatchExpr(x) => x.into_node(),
        Self::Path(x) => x.into_node(),
        Self::ReturnExpr(x) => x.into_node(),
        Self::TupleExpr(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{, | cons_pattern | group_pattern | literal | parameter | rest_pattern}`:\n- [symbols::Comma]\n- [ConsPattern]\n- [GroupPattern]\n- [Literal]\n- [Parameter]\n- [RestPattern]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern<'tree> {
    Comma(symbols::Comma<'tree>),
    ConsPattern(ConsPattern<'tree>),
    GroupPattern(GroupPattern<'tree>),
    Literal(Literal<'tree>),
    Parameter(Parameter<'tree>),
    RestPattern(RestPattern<'tree>),
  }
  #[automatically_derived]
  impl<'tree> Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern<'tree> {
    #[doc = "Returns the node if it is of kind `,` ([symbols::Comma]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn comma(self) -> Option<symbols::Comma<'tree>> {
      match self {
        Self::Comma(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `cons_pattern` ([ConsPattern]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn cons_pattern(self) -> Option<ConsPattern<'tree>> {
      match self {
        Self::ConsPattern(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `group_pattern` ([GroupPattern]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn group_pattern(self) -> Option<GroupPattern<'tree>> {
      match self {
        Self::GroupPattern(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `literal` ([Literal]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn literal(self) -> Option<Literal<'tree>> {
      match self {
        Self::Literal(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `parameter` ([Parameter]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn parameter(self) -> Option<Parameter<'tree>> {
      match self {
        Self::Parameter(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `rest_pattern` ([RestPattern]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn rest_pattern(self) -> Option<RestPattern<'tree>> {
      match self {
        Self::RestPattern(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "," => Ok(unsafe {
          Self::Comma(<symbols::Comma<'tree> as type_sitter_lib::TypedNode<
            'tree,
          >>::from_node_unchecked(node))
        }),
        "cons_pattern" => Ok(unsafe {
          Self::ConsPattern(
            <ConsPattern<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "group_pattern" => Ok(unsafe {
          Self::GroupPattern(
            <GroupPattern<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "literal" => Ok(unsafe {
          Self::Literal(
            <Literal<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "parameter" => Ok(unsafe {
          Self::Parameter(
            <Parameter<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "rest_pattern" => Ok(unsafe {
          Self::RestPattern(
            <RestPattern<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for Comma_ConsPattern_GroupPattern_Literal_Parameter_RestPattern<'tree>
  {
    const KIND: &'static str =
      "{, | cons_pattern | group_pattern | literal | parameter | rest_pattern}";

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node(),
        Self::ConsPattern(x) => x.node(),
        Self::GroupPattern(x) => x.node(),
        Self::Literal(x) => x.node(),
        Self::Parameter(x) => x.node(),
        Self::RestPattern(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.node_mut(),
        Self::ConsPattern(x) => x.node_mut(),
        Self::GroupPattern(x) => x.node_mut(),
        Self::Literal(x) => x.node_mut(),
        Self::Parameter(x) => x.node_mut(),
        Self::RestPattern(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::Comma(x) => x.into_node(),
        Self::ConsPattern(x) => x.into_node(),
        Self::GroupPattern(x) => x.into_node(),
        Self::Literal(x) => x.into_node(),
        Self::Parameter(x) => x.into_node(),
        Self::RestPattern(x) => x.into_node(),
      }
    }
  }
  #[doc = "one of `{class_decl | clause | command | data_decl | instance_decl | signature | trait_decl | type_decl | using}`:\n- [ClassDecl]\n- [Clause]\n- [Command]\n- [DataDecl]\n- [InstanceDecl]\n- [Signature]\n- [TraitDecl]\n- [TypeDecl]\n- [Using]"]
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  #[allow(non_camel_case_types)]
  pub enum ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using<'tree> {
    ClassDecl(ClassDecl<'tree>),
    Clause(Clause<'tree>),
    Command(Command<'tree>),
    DataDecl(DataDecl<'tree>),
    InstanceDecl(InstanceDecl<'tree>),
    Signature(Signature<'tree>),
    TraitDecl(TraitDecl<'tree>),
    TypeDecl(TypeDecl<'tree>),
    Using(Using<'tree>),
  }
  #[automatically_derived]
  impl<'tree>
    ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using<'tree>
  {
    #[doc = "Returns the node if it is of kind `class_decl` ([ClassDecl]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn class_decl(self) -> Option<ClassDecl<'tree>> {
      match self {
        Self::ClassDecl(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `clause` ([Clause]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn clause(self) -> Option<Clause<'tree>> {
      match self {
        Self::Clause(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `command` ([Command]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn command(self) -> Option<Command<'tree>> {
      match self {
        Self::Command(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `data_decl` ([DataDecl]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn data_decl(self) -> Option<DataDecl<'tree>> {
      match self {
        Self::DataDecl(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `instance_decl` ([InstanceDecl]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn instance_decl(self) -> Option<InstanceDecl<'tree>> {
      match self {
        Self::InstanceDecl(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `signature` ([Signature]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn signature(self) -> Option<Signature<'tree>> {
      match self {
        Self::Signature(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `trait_decl` ([TraitDecl]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn trait_decl(self) -> Option<TraitDecl<'tree>> {
      match self {
        Self::TraitDecl(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `type_decl` ([TypeDecl]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn type_decl(self) -> Option<TypeDecl<'tree>> {
      match self {
        Self::TypeDecl(x) => Some(x),
        _ => None,
      }
    }

    #[doc = "Returns the node if it is of kind `using` ([Using]), otherwise returns None"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn using(self) -> Option<Using<'tree>> {
      match self {
        Self::Using(x) => Some(x),
        _ => None,
      }
    }
  }
  #[automatically_derived]
  impl<'tree> TryFrom<tree_sitter::Node<'tree>>
    for ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using<'tree>
  {
    type Error = type_sitter_lib::IncorrectKind<'tree>;

    #[inline]
    fn try_from(node: tree_sitter::Node<'tree>) -> Result<Self, Self::Error> {
      match node.kind() {
        "class_decl" => Ok(unsafe {
          Self::ClassDecl(
            <ClassDecl<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "clause" => Ok(unsafe {
          Self::Clause(
            <Clause<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "command" => Ok(unsafe {
          Self::Command(
            <Command<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "data_decl" => Ok(unsafe {
          Self::DataDecl(
            <DataDecl<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "instance_decl" => Ok(unsafe {
          Self::InstanceDecl(
            <InstanceDecl<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "signature" => Ok(unsafe {
          Self::Signature(
            <Signature<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "trait_decl" => Ok(unsafe {
          Self::TraitDecl(
            <TraitDecl<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "type_decl" => Ok(unsafe {
          Self::TypeDecl(
            <TypeDecl<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        "using" => Ok(unsafe {
          Self::Using(
            <Using<'tree> as type_sitter_lib::TypedNode<'tree>>::from_node_unchecked(node),
          )
        }),
        _ => Err(type_sitter_lib::IncorrectKind {
          node,
          kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
        }),
      }
    }
  }
  #[automatically_derived]
  impl<'tree> type_sitter_lib::TypedNode<'tree>
    for ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using<'tree>
  {
    const KIND : & 'static str = "{class_decl | clause | command | data_decl | instance_decl | signature | trait_decl | type_decl | using}" ;

    #[inline]
    fn node(&self) -> &tree_sitter::Node<'tree> {
      match self {
        Self::ClassDecl(x) => x.node(),
        Self::Clause(x) => x.node(),
        Self::Command(x) => x.node(),
        Self::DataDecl(x) => x.node(),
        Self::InstanceDecl(x) => x.node(),
        Self::Signature(x) => x.node(),
        Self::TraitDecl(x) => x.node(),
        Self::TypeDecl(x) => x.node(),
        Self::Using(x) => x.node(),
      }
    }

    #[inline]
    fn node_mut(&mut self) -> &mut tree_sitter::Node<'tree> {
      match self {
        Self::ClassDecl(x) => x.node_mut(),
        Self::Clause(x) => x.node_mut(),
        Self::Command(x) => x.node_mut(),
        Self::DataDecl(x) => x.node_mut(),
        Self::InstanceDecl(x) => x.node_mut(),
        Self::Signature(x) => x.node_mut(),
        Self::TraitDecl(x) => x.node_mut(),
        Self::TypeDecl(x) => x.node_mut(),
        Self::Using(x) => x.node_mut(),
      }
    }

    #[inline]
    fn into_node(self) -> tree_sitter::Node<'tree> {
      match self {
        Self::ClassDecl(x) => x.into_node(),
        Self::Clause(x) => x.into_node(),
        Self::Command(x) => x.into_node(),
        Self::DataDecl(x) => x.into_node(),
        Self::InstanceDecl(x) => x.into_node(),
        Self::Signature(x) => x.into_node(),
        Self::TraitDecl(x) => x.into_node(),
        Self::TypeDecl(x) => x.into_node(),
        Self::Using(x) => x.into_node(),
      }
    }
  }
}
