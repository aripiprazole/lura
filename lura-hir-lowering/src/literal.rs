//! Defines a module for resolving language "literals", that are simple values, like numbers,
//! strings, and other things. It will be used in the resolution, and it's a helper module for the
//! [`LowerHir`] struct.
//!
//! It's only a module, to organization purposes.

use lura_hir::source::literal::Literal;

use super::*;

impl HirLowering<'_, '_> {
  pub fn literal(&mut self, tree: lura_syntax::Literal) -> Literal {
    use lura_syntax::anon_unions::Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8::*;

    let text = tree
      .utf8_text(self.src.source_text(self.db).as_bytes())
      .unwrap_or_default();

    tree.child().with_db(self, |_, node| match node {
      Char(..) => todo!("Not implemented Char literal"),
      F32(..) => todo!("Not implemented F32 literal"),
      F64(..) => todo!("Not implemented F64 literal"),
      I8(..) => text.parse::<i8>().ok().map(Literal::Int8),
      I16(..) => text.parse::<i16>().ok().map(Literal::Int16),
      I64(..) => text.parse::<i64>().ok().map(Literal::Int64),
      I128(..) => todo!("Not implemented I128 literal"),
      U1(..) => text.parse::<bool>().ok().map(Literal::Boolean),
      U8(..) => text.parse::<u8>().ok().map(Literal::UInt8),
      U16(..) => text.parse::<u16>().ok().map(Literal::UInt16),
      U32(..) => text.parse::<u32>().ok().map(Literal::UInt32),
      U64(..) => text.parse::<u64>().ok().map(Literal::UInt64),
      U128(..) => todo!("Not implemented U128 literal"),
      Nat(..) => todo!("Not implemented Nat literal"),
      String(..) => Some(Literal::String((&text[1..text.len() - 1]).into())),
    })
  }
}
