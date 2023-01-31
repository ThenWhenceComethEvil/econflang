-module(scanner).
-export([
         scan/0
        ]).


-define(PROGDIR,
        filename:join([ filename:dirname(?FILE), "../" ])).
-define(INPUT,
        filename:join([ ?PROGDIR, "main.conf" ])).


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

-define(ADV_LINE(C),
        #cursor{ index  = 1 + C#cursor.index,
                 lineno = 1 + C#cursor.lineno,
                 colno  = 1 }).
-define(ADV_INDEX(C, N),
        #cursor{ index  = N + C#cursor.index,
                 lineno =     C#cursor.lineno,
                 colno  = N + C#cursor.colno }).
-define(ADV_INDEX(C),
        ?ADV_INDEX(C, 1)).

-define(IS_WORDSTART(C),
        ((C =:= $_)                     orelse
         ((C >= $a) andalso (C =< $z))  orelse
         ((C >= $A) andalso (C =< $Z)))).
-define(IS_WORDCHAR(C),
        (?IS_WORDSTART(C)               orelse 
         ((C >= $0) andalso (C =< $9)))).


scan() ->
   {ok, Binary} = file:read_file(?INPUT),
   parse_char(binary_to_list(Binary)).


parse_char(Input) -> parse_char(Input, #cursor{}, []).
parse_char([], _, Tokens) ->
   lists:reverse(Tokens);
parse_char(Chars, Cursor, Acc) ->
   {Token, Rest} = toke(Chars, Cursor),
   parse_char(Rest, Token#token.end_cursor, [Token|Acc]).

% No input left. Eof token.
toke([], Cursor) ->
   {#token{
       type         = eof,
       start_cursor = Cursor,
       end_cursor   = ?ADV_LINE(Cursor)
      }, []};

% Whitespace tokens.
toke([ $#  |T], Cursor) -> strip_comment(T, ?ADV_INDEX(Cursor));
toke([ $\n |T], Cursor) -> toke(T, ?ADV_LINE(Cursor));
toke([ $\s |T], Cursor) -> toke(T, ?ADV_INDEX(Cursor));
toke([ $\t |_],      _) -> error(no_tabs_for_you);

% Strings.
toke([ $'  |T], Cursor) -> toke_path(T, Cursor);
toke([ $"  |T], Cursor) -> toke_string(T, Cursor);

% Identifiers.
toke([H|T], Cursor) when ?IS_WORDSTART(H) ->
   toke_identifier(T, Cursor, Cursor, [H]);

% Single-character tokens.
toke([H|T], Cursor) ->
   Token_t = case H of
      $.  -> dot           ;
      $:  -> colon         ;
      $@  -> at_symbol     ;
      $(  -> left_paren    ;
      $)  -> right_paren   ;
      ${  -> left_brace    ;
      $}  -> right_brace   ;
      $[  -> left_bracket  ;
      $]  -> right_bracket ;
      _   -> error(invalid_char)
   end,
   {#token{
       type         = Token_t,
       value        = [H],
       start_cursor = Cursor,
       end_cursor   = Cursor
      }, T}.


strip_comment([ $\n |T], Cursor) ->
   toke(T, ?ADV_LINE(Cursor));
strip_comment([ _ |T], Cursor) ->
   strip_comment(T, ?ADV_INDEX(Cursor)).


toke_path(Li, Cursor) ->
   toke_path(Li, Cursor, Cursor, []).

toke_path([], _, _, _) ->
   error(unterminated_path);
toke_path([ $' |T], Start, End, Text) ->
   {#token{
       type         = path,
       value        = lists:reverse(Text),
       start_cursor = Start,
       end_cursor   = End
      }, T};
toke_path([ $\\, $' |T], Start, End, Text) ->
   toke_path(T, Start, ?ADV_INDEX(End), [$' |Text]);
toke_path([ H |T], Start, End, Text) ->
   toke_path(T, Start, ?ADV_INDEX(End), [H |Text]).


toke_string(Li, Cursor) ->
   toke_string(Li, Cursor, Cursor, []).

toke_string([], _, _, _) ->
   error(unterminated_string);
toke_string([ $' |T], Start, End, Text) ->
   {#token{
       type         = string,
       value        = lists:reverse(Text),
       start_cursor = Start,
       end_cursor   = End
      }, T};
toke_string([ $\\, $' |T], Start, End, Text) ->
   toke_string(T, Start, ?ADV_INDEX(End, 2), [$' |Text]);
toke_string([ H |T], Start, End, Text) ->
   toke_string(T, Start, ?ADV_INDEX(End), [H |Text]).


toke_identifier([H|T], Start, End, Text) when ?IS_WORDCHAR(H) ->
   toke_identifier(T, Start, ?ADV_INDEX(End), [H|Text]);
toke_identifier(Rest, Start, End, Text) ->
   {#token{
       type         = identifier,
       value        = lists:reverse(Text),
       start_cursor = Start,
       end_cursor   = End
      }, Rest}.
