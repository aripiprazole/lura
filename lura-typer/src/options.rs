/// Options to configure the typer, like custom type features
/// or custom type inference.
///
/// This is a work in progress.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TyperFeatures {
    /// Enables the full dependent types, which allows
    /// the language to use types as values, and values as
    /// types.
    ///
    /// This enables the full engine, that was wrote using
    /// dependent types.
    pub dependent_types: bool,

    /// Type families, which are types that are parametrized
    /// by values.
    ///
    /// This is a feature that use dependent types, and isn't
    /// compatible with [`TyperFeatures::dependent_types`].
    ///
    /// Also enables GADT, as GADT is a special case of type
    /// families.
    pub type_families: bool,

    /// Enables subsumption, which is a form of subtyping
    /// that allows for more flexible type inference.
    ///
    /// This is intended to use with dependent types, and
    /// unifying forall types.
    pub first_class_polymorphic_types: bool,
}
