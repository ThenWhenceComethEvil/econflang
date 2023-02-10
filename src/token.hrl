-record(cursor, {
          index  = 1,
          lineno = 1,
          colno  = 0
         }).

-record(token, {
          type         = none,
          value        = none,
          start_cursor = none,
          end_cursor   = none
         }).
