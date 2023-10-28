(f32) @number
(f64) @number
(u1) @number
(i8) @number
(u8) @number
(i16) @number
(u16) @number
(u64) @number
(i64) @number
(u64) @number
(i128) @number
(u128) @number
(nat) @number
(string) @string
(char) @string

["." ","] @punctuation.delimiter

["<-" "->" "=>"] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "|"
]  @punctuation.bracket

[
  "if"
  "return"
  "class"
  "trait"
  "data"
  "public"
  "sealed"
  "private"
  "internal"
  "match"
  "then"
  "else"
  "let"
  "using"
  "is"
  "type"
] @keyword

(line_comment) @comment
(doc_string) @comment

"@" @attribute

(signature name: (path) @function.method)
(clause name: (path) @function.method)

(parameter
  pattern: (cons_pattern))

(cons_pattern name: (path) @variable)

(attribute name: (path) @attribute)