module.exports = grammar({
  name: 'lura',

  extras: $ => [
    $.line_comment,
    /[\s\r\n\uFEFF\u2060\u200B]/,
  ],

  conflicts: $ => [
    [$.cons_pattern, $.primary],
    [$.app_expr, $._expr],
    [$.if_stmt, $.if_expr],
    [$.type_app_expr, $.app_expr],
    [$._expr, $.type_app_expr, $.app_expr],
    [$._type_expr, $.primary],
    [$._pattern, $.primary],
    [$.forall_parameter, $.free_variable],
  ],

  precedences: $ => [
    [
      $.string,
      $.char,
      $.f32,
      $.f64,
      $.i32,
      $.u32,
      $.u1,
      $.i8,
      $.u8,
      $.i16,
      $.u16,
      $.i64,
      $.u64,
      $.i128,
      $.u128,
      $.nat,
    ],
    [$.sigma_expr, $.array_expr],
    [$.primary, $._expr],
    [$._type_expr, $.primary],
    [$._type_expr, $.type_app_expr, $._expr, $.app_expr],
    [$._arm_body, $.sigma_expr, $.binary_expr],
    [$._arm_body, $.ann_expr],
    [$._pi_parameter_set, $.ann_expr],
    [$._pi_parameter_set, $.pi_expr],
    [$._pi_parameter_set, $.sigma_expr],
    [$._pi_parameter_set, $.forall_expr],
    [$.path, $.primary],
  ],

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => seq(
      optional(field('hash_bang', $.hash_bang)),
      optional(seq(
        field('decl', $._decl),
        repeat(seq($._line_break, field('decl', $._decl))),
        optional($._line_break)
      ))
    ),

    symbol_identifier: $ => seq('`', $.infix_op),

    identifier: $ => choice($.simple_identifier, $.symbol_identifier),

    path: $ => prec.right(seq(
      field('segment', $.identifier),
      repeat(seq('.', field('segment', $.identifier))),
    )),
    // Declarations

    _decl: $ => choice(
      $.command,
      $.using,
      $.class_decl,
      $.trait_decl,
      $.data_decl,
      $.type_decl,
      $.instance_decl,
      $.signature,
      $.clause,
    ),

    _argument_list: $ => choice($.explicit_arguments, $.implicit_arguments),

    attribute: $ => prec.left(seq(
      '@',
      field('name', $.path),
      optional(seq(
        '(',
        $._expr,
        repeat(seq(',', field('argument', $._expr))),
        optional(','),
        ')',
      )),
    )),

    explicit_arguments: $ => seq('(', optional($._parameter_set), ')'),

    implicit_arguments: $ => seq('[', optional($._parameter_set), ']'),

    visibility: $ => choice('public', 'sealed', 'private', 'internal'),

    using: $ => seq(
      repeat(field('attribute', $.attribute)),
      'using',
      field('path', $.path),
    ),

    command: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      '#',
      field('command', $.path),
      optional(seq(
        field('argument', $._expr),
        repeat(seq(',', field('argument', $._expr))),
        optional(','),
      )),
    ),

    signature: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      optional(field('visibility', $.visibility)),
      field('name', $.path),
      repeat(field('argument', $._argument_list)),
      optional(field('clause_type', $.clause_type)),
      optional(field('value', $.block)),
    ),

    clause: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      field('name', $.path),
      repeat(field('pattern', $._pattern)),
      '=',
      optional(field('value', $._expr)),
    ),

    instance_decl: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      optional(field('visibility', $.visibility)),
      'instance',
      field('name', $.path),
      repeat(field('argument', $._argument_list)),
      'of',
      field('item', $.primary),
      repeat(seq(',', field('item', $.primary))),
      optional(','),
      optional(field('trait_body', $._class_body)),
    ),

    type_decl: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      optional(field('visibility', $.visibility)),
      'type',
      field('name', $.path),
      repeat(field('argument', $._argument_list)),
      optional(field('clause_type', $.clause_type)),
      optional(seq('=', field('value', $._type_expr))),
    ),

    data_decl: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      optional(field('visibility', $.visibility)),
      'data',
      field('name', $.path),
      repeat(field('argument', $._argument_list)),
      optional(field('clause_type', $.clause_type)),
      optional(field('data_body', $._data_body)),
    ),

    trait_decl: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      optional(field('visibility', $.visibility)),
      'trait',
      field('name', $.path),
      repeat(field('argument', $._argument_list)),
      optional(field('clause_type', $.clause_type)),
      optional(field('trait_body', $._class_body)),
    ),

    class_decl: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      optional(field('visibility', $.visibility)),
      'class',
      field('name', $.path),
      repeat(field('argument', $._argument_list)),
      optional(field('clause_type', $.clause_type)),
      optional(field('class_body', $._class_body)),
    ),

    _class_body: $ => seq(
      '{',
      optional(seq(
        field('field', $._class_property),
        repeat(seq($._line_break, field('field', $._class_property))),
        optional($._line_break)
      )),
      '}'
    ),

    _class_property: $ => choice($.signature),

    clause_type: $ => seq(':', field('clause_type', $._type_expr)),

    _data_body: $ => seq(
      '{',
      optional(field('constructor', $._data_constructors)),
      optional(seq(';', field('method', $._data_methods))),
      '}'
    ),

    _data_constructor: $ => choice($.signature_constructor, $.function_constructor),

    _data_constructors: $ => seq(
      field('constructor', $._data_constructor),
      repeat(seq(',', field('constructor', $._data_constructor))),
      optional(',')
    ),

    _data_methods: $ => seq(
      field('field', $._class_property),
      repeat(seq($._line_break, field('field', $._class_property))),
      optional($._line_break)
    ),

    signature_constructor: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      field('name', $.path),
      ':',
      field('field_type', $._type_expr),
    ),

    function_constructor: $ => seq(
      repeat(field('doc_string', $.doc_string)),
      repeat(field('attribute', $.attribute)),
      field('name', $.path),
      optional(seq(
        '(',
        field('parameter', $._type_expr),
        repeat(seq(',', field('parameter', $._type_expr))),
        optional(','),
        ')',
      )),
    ),

    // Statements
    _stmt: $ => choice(
      $.let_stmt,
      $.if_stmt,
      $.ask_stmt,
      $.expr_stmt,
    ),

    if_stmt: $ => seq(
      'if',
      field('condition', $._expr),
      field('then', $.then_body),
      optional(field('otherwise', $.otherwise_body)),
    ),

    ask_stmt: $ => seq(
      field('pattern', $._pattern),
      '<-',
      field('value', $._expr),
    ),

    let_stmt: $ => seq(
      'let',
      field('pattern', $._pattern),
      '=',
      field('value', $._expr),
    ),

    expr_stmt: $ => $._expr,

    block: $ => seq(
      '{',
      optional(seq(
        field('statement', $._stmt),
        repeat(seq($._line_break, field('statement', $._stmt))),
        optional($._line_break)
      )),
      '}',
    ),

    // Patterns
    _pattern: $ => choice($.group_pattern, $.cons_pattern, $.rest_pattern, $.literal),

    rest_pattern: $ => '..',

    cons_pattern: $ => prec.left(seq(
      field('name', $.path),
      repeat(field('pattern', $._pattern)),
    )),

    group_pattern: $ => prec.left(seq(
      '(',
      field('pattern', $._pattern),
      ')',
    )),

    // Expressions
    _expr: $ => choice(
      $.primary,
      $.match_expr,
      $.sigma_expr,
      $.app_expr,
      $.lam_expr,
      $.ann_expr,
      $.pi_expr,
      $.binary_expr,
      $.forall_expr,
    ),

    _type_expr: $ => choice(
      $.primary,
      $.match_expr,
      $.sigma_expr,
      $.type_app_expr,
      $.lam_expr,
      $.ann_expr,
      $.pi_expr,
      $.binary_expr,
      $.forall_expr,
    ),

    forall_expr: $ => prec.left(seq(
      'forall',
      repeat1(field('parameter', $.forall_parameter)),
      '.',
      field('value', $._type_expr),
    )),

    binary_expr: $ => prec.left(seq(
      field('lhs', $._expr),
      field('op', $.infix_op),
      field('rhs', $._expr),
    )),

    type_app_expr: $ => prec.left(seq(
      field('callee', $.primary),
      repeat(field('argument', $.primary)),
    )),

    app_expr: $ => prec.left(seq(
      field('callee', $.primary),
      repeat(field('argument', $.primary)),
      optional($.block),
    )),

    tuple_expr: $ => seq(
      '(',
      optional(seq(
        $._expr,
        repeat(seq(',', $._expr)),
        optional(','),
      )),
      ')',
    ),

    array_expr: $ => seq(
      '[',
      optional(seq(
        field('item', $._expr),
        repeat(seq(',', field('item', $._expr))),
        optional(','),
      )),
      ']'
    ),

    ann_expr: $ => prec.left(seq(
      field('value', $._expr),
      'is',
      field('against', $._type_expr),
    )),

    parameter: $ => prec.left(seq(
      optional(seq(field('pattern', $._pattern), ':')),
      field('parameter_type', $._type_expr),
    )),

    _parameter_set: $ => prec.left(seq(
      field('parameter', $._any_parameter),
      repeat(seq(',', field('parameter', $._any_parameter))),
      optional(','),
    )),

    _type_parameter_set: $ => prec.left(seq(
      field('parameter', $._type_parameter),
      repeat(seq(',', field('parameter', $._type_parameter))),
      optional(','),
    )),

    forall_parameter: $ => prec.left(seq(
      '^',
      field('identifier', $.identifier),
    )),

    _type_parameter: $ => choice(
      $._pattern,
      $.parameter,
    ),

    _any_parameter: $ => choice(
      $.forall_parameter,
      $.parameter,
    ),

    lam_expr: $ => prec.left(seq(
      '|',
      optional(field('parameter', $._parameter_set)),
      '|',
      field('value', $._expr),
    )),

    pi_named_parameter_set: $ => seq(
      '(',
      field('parameter', $._parameter_set),
      ')',
    ),

    _pi_parameter_set: $ => choice(
      $.pi_named_parameter_set,
      $._type_expr,
    ),

    pi_expr: $ => prec.left(seq(
      field('parameter', $._pi_parameter_set),
      '->',
      field('value', $._type_expr),
    )),

    sigma_expr: $ => prec.left(seq(
      '[',
      field('parameter', $._type_parameter_set),
      ']',
      '=>',
      field('value', $._type_expr),
    )),

    if_expr: $ => seq(
      'if',
      field('condition', $._expr),
      field('then', $.then_body),
      field('otherwise', $.otherwise_body),
    ),

    match_expr: $ => seq(
      'match',
      field('scrutinee', $._expr),
      '{',
      repeat(field('arm', $.match_arm)),
      '}',
    ),

    return_expr: $ => prec.left(seq(
      'return',
      optional(field('value', $._expr)),
    )),

    match_arm: $ => seq(
      field('pattern', $._pattern),
      '=>',
      field('body', $._arm_body),
    ),

    then_body: $ => prec.left(choice(
      $.block,
      seq('then', $._expr),
    )),

    otherwise_body: $ => prec.left(seq(
      'else',
      field('value', $._arm_body)
    )),

    _arm_body: $ => choice($.block, $._expr),

    free_variable: $ => seq('^', $.identifier),

    // Primaries
    primary: $ => choice(
      $.literal,
      $.path,
      $.free_variable,
      $.tuple_expr,
      $.array_expr,
      $.if_expr,
      $.match_expr,
      $.return_expr,
    ),

    literal: $ => choice(
      $.string,
      $.char,
      $.f32,
      $.f64,
      $.u1,
      $.i8,
      $.u8,
      $.i16,
      $.u16,
      $.u32,
      $.i64,
      $.u64,
      $.i128,
      $.u128,
      $.nat,
    ),

    _integer: $ => choice($._decimal, $.octal, $.hex, $.binary),

    f32: $ => seq($._float, optional('f32')),
    f64: $ => seq($._float, optional('f64')),
    i32: $ => seq($._integer, optional('u32')),
    u32: $ => seq($._integer, optional('u32')),
    u1: $ => seq($._integer, optional('u1')),
    i8: $ => seq($._integer, optional('i8')),
    u8: $ => seq($._integer, optional('u8')),
    i16: $ => seq($._integer, optional('i16')),
    u16: $ => seq($._integer, optional('u16')),
    i64: $ => seq($._integer, optional('i64')),
    u64: $ => seq($._integer, optional('u64')),
    i128: $ => seq($._integer, optional('i128')),
    u128: $ => seq($._integer, optional('u128')),
    nat: $ => seq($._integer, optional('n')),

    octal: $ => seq(/Oo/i, $._octal),
    hex: $ => seq(/Ox/i, $._hex),
    binary: $ => seq(/Ob/i, $._binary),

    // LEXER
    _line_break: $ => /(\n|\r\n|;)+/,

    _symbol: $ => choice('$', '?', '.', '+', '-', '*', '/', '%', '&', '|', '&&', '||', '!', '~', '=', '<', '>'),

    _octal: $ => /[0-7]+/i,
    _hex: $ => /[0-8a-fA-F]+/i,
    _binary: $ => /[0-1]+/i,
    _decimal: $ => /[0-9]+/i,
    _float: $ => /\d+(\.[\d_]+)?/,

    char: $ => /'[^'\\]'/,
    string: $ => /"([^"\\\n\r]|\\[^\n\r])*"/,

    infix_op: $ => prec.left(repeat1($._symbol)),

    attribute_id: $ => /[a-zA-Z][a-zA-Z\d_$]*/,

    hash_bang: $ => /#!.*/,

    doc_string: $ => prec(2, token(seq('//!', /.*/))),
    line_comment: $ => prec(1, token(seq('//', /.*/))),

    simple_identifier: $ => /[a-zA-Z_'][a-zA-Z'\d_$]*/,
  }
});
