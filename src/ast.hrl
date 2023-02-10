-record(decl, {
          name         = none,
          expr         = none,
          start_cursor = none,
          end_cursor   = none
         }).

-record(expr, {
          kind         = none,
          lhs          = none,
          rhs          = none,
          op           = none,
          type         = none,
          start_cursor = none,
          end_cursor   = none
         }).
