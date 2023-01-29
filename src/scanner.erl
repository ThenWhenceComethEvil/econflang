-module(scanner).
-export([
         scan/0
        ]).


-define(PROGDIR,
        filename:join([ filename:dirname(?FILE), "../" ])).
-define(INPUT,
        filename:join([ ?PROGDIR, "main.conf" ])).

-record(cursor, {
          index     = 1,
          lineno    = 1,
          colno     = 0
         }).

-record(token, {
          type         = none,
          value        = none,
          cursor       = none
         }).

-define(ADV_LINE(C),
        #cursor{ index  = 1 + C#cursor.index,
                 lineno = 1 + C#cursor.lineno,
                 colno  = 1 }).
-define(ADV_INDEX(C),
        #cursor{ index  = 1 + C#cursor.index,
                 lineno =     C#cursor.lineno,
                 colno  = 1 + C#cursor.lineno }).
-define(ADV_INDEX(C, N),
        #cursor{ index  = N + C#cursor.index,
                 lineno =     C#cursor.lineno,
                 colno  = N + C#cursor.lineno }).


scan() ->
   {ok, Binary} = file:read_file(?INPUT),
   parse_char(binary_to_list(Binary)).


parse_char(Input) -> parse_char(Input, #cursor{}, []).
parse_char([], _, Tokens) ->
   lists:reverse(Tokens);
parse_char(Chars, Cursor, Acc) ->
   {Token, Rest} = toke(Chars, Cursor),
   parse_char(Rest, Token#token.cursor, [Token|Acc]).


% No input left. Eof token.
toke([], Cursor) ->
   {#token{type=eof, cursor=?ADV_LINE(Cursor)}, []};

% Whitespace tokens.
toke([ $#  |T], Cursor) -> strip_comment(T, ?ADV_INDEX(Cursor));
toke([ $\n |T], Cursor) -> toke(T, ?ADV_LINE(Cursor));
toke([ $\s |T], Cursor) -> toke(T, ?ADV_INDEX(Cursor));
toke([ $\t |_],      _) -> error(no_tabs_for_you);

% Strings.
toke([ $'  |T], Cursor) -> toke_path(T, Cursor);
toke([ $"  |T], Cursor) -> toke_string(T, Cursor);

% Single-character tokens.
toke([H|T], Cursor) ->
   Token_t = case H of
      ${  -> left_brace;
      $}  -> right_brace;
      $(  -> left_paren;
      $)  -> right_paren;
      $@  -> at_symbol;
      _   -> io:format("Invalid: [~s]~n", [[H]]),
             invalid_char
   end,
   {#token{type=Token_t, value=H, cursor=Cursor}, T}.

strip_comment([ $\n |T] , Cursor) -> toke(T, ?ADV_LINE(Cursor));
strip_comment([   _ |T] , Cursor) -> strip_comment(T, ?ADV_INDEX(Cursor)).

toke_path(Li, Cursor) -> toke_path(Li, Cursor, []).
toke_path([           ],      _,    _) -> error(unterminated_path);
toke_path([      $' |T], Cursor, Text) ->
   {#token{type=path, value=lists:reverse(Text), cursor=Cursor}, T};
toke_path([ $\\, $' |T], Cursor, Text) -> toke_path(T, ?ADV_INDEX(Cursor), [$' |Text]);
toke_path([       H |T], Cursor, Text) -> toke_path(T, ?ADV_INDEX(Cursor), [ H |Text]).

toke_string(Li, Cursor) -> toke_string(Li, Cursor, []).
toke_string([           ],      _,    _) -> error(unterminated_string);
toke_string([      $' |T], Cursor, Text) ->
   {#token{type=string, value=lists:reverse(Text), cursor=Cursor}, T};
toke_string([ $\\, $' |T], Cursor, Text) -> toke_string(T, ?ADV_INDEX(Cursor, 2), [$' |Text]);
toke_string([       H |T], Cursor, Text) -> toke_string(T, ?ADV_INDEX(Cursor)   , [ H |Text]).
