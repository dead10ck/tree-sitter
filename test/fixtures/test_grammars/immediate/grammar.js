// This grammar demonstrates the usage of the IMMEDIATE rule. It allows the parser to produce
// a different token based on whether or not there are `extras` preceding the rule's main content.

module.exports = grammar({
  name: "immediate",

  extras: (_) => [/\s/],

  rules: {
    program: ($) =>
      seq($._expression, repeat(seq(/\s/, optional($._expression)))),

    _expression: ($) => choice($.num, $.num_seq),

    num: (_) => /\d+/,

    // ..1..5..20..25..100..
    num_seq: ($) =>
      choice(
        // ..10
        $._left_open,

        // 1..10
        // ..1..10
        // ..1..10..
        // 1..10..
        seq(
          choice($.num, $._left_open),
          repeat1(immediate($._num_seq_tail)),
          optional(immediate($.inf)),
        ),

        // 1..
        $._right_open,
      ),

    inf: (_) => "..",

    _left_open: ($) => seq($.inf, immediate($.num)),
    _num_seq_tail: ($) => seq("..", immediate($.num)),
    _right_open: ($) => seq($.num, immediate($.inf)),
  },
});
