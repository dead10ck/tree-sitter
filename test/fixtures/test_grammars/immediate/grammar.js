// This grammar demonstrates the usage of the IMMEDIATE rule. It allows the parser to produce
// a different token based on whether or not there are `extras` preceding the rule's main content.

module.exports = grammar({
  name: "immediate",

  extras: (_) => [/\s/],

  rules: {
    program: ($) => repeat(seq($._expression, immediate($._terminator))),

    _expression: ($) => choice($.num, $.num_seq),

    num: (_) => /\d+/,

    num_seq: ($) =>
      choice(
        seq(
          $.num,
          choice(
            // 1.
            // 1..
            immediate($._end),

            // 1.10
            // 1.10.
            seq(
              repeat1(immediate($._num_seq_tail)),
              optional(immediate($._end)),
            ),

            // 1.10..
            seq(repeat(immediate($._num_seq_tail)), immediate($._end)),
          ),
        ),

        seq(
          // .1
          // ..1
          choice($._left_open, $._num_seq_tail),

          optional(
            choice(
              // .1.
              // .1..
              // ..1.
              // ..1..
              // .1.10.
              // ..1.10.
              // ..1.10..
              seq(repeat(immediate($._num_seq_tail)), immediate($._end)),

              // .1.10
              // .1.10.
              // .1.10..
              // ..1.10
              // ..1.10.
              // ..1.10..
              seq(
                repeat1(immediate($._num_seq_tail)),
                optional(immediate($._end)),
              ),
            ),
          ),
        ),
      ),

    inf: (_) => "..",
    _sep: (_) => ".",
    _end: ($) => choice($._sep, $.inf),
    _terminator: (_) => /\n/,

    _left_open: ($) => seq($.inf, immediate($.num)),
    _num_seq_tail: ($) => seq($._sep, immediate($.num)),
    _right_open: ($) => seq($.num, immediate($.inf)),
  },
});
