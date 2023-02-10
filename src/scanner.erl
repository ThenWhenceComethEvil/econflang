-module(scanner).
-export([ scan/0 ]).
-include("token.hrl").


-define(PROGDIR,
        filename:join([ filename:dirname(?FILE), "../" ])).
-define(INPUT,
        filename:join([ ?PROGDIR, "sample", "main.conf" ])).


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

-define(IS_NUMBER(C),
        (C >= $0) andalso (C =< $9)).


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
       end_cursor   = Cursor
      }, []};

% Whitespace tokens.
toke([ $#  |T], Cursor) -> strip_comment(T, ?ADV_INDEX(Cursor));
toke([ $\n |T], Cursor) -> toke(T, ?ADV_LINE(Cursor));
toke([ $\s |T], Cursor) -> toke(T, ?ADV_INDEX(Cursor));
toke([ $\t |_],      _) -> error(no_tabs_for_you);


%% F-strings & f-paths.
%toke([ $f, $' |T], Cursor) -> tokenize_fpath(T, ?ADV_LINE(Cursor, 2));
%toke([ $f, $" |T], Cursor) -> tokenize_fstring(T, ?ADV_LINE(Cursor, 2));


%% Strings.
%toke([ $'  |T], Cursor) -> path(T, ?ADV_INDEX(Cursor));
%toke([ $"  |T], Cursor) -> string(T, ?ADV_INDEX(Cursor));

%% F-strings
%toke([ $f, $'  |T], Cursor) -> fpath(T, ?ADV_INDEX(Cursor, 2));
%toke([ $f, $"  |T], Cursor) -> fstring(T, ?ADV_INDEX(Cursor, 2));


% Identifiers.
toke(Li=[H|_], Cursor) when ?IS_WORDSTART(H) ->
   identifier(Li, ?ADV_INDEX(Cursor));


% Numbers.
toke(Li=[H|_], Cursor) when ?IS_NUMBER(H) ->
   number(Li, ?ADV_INDEX(Cursor));


%% Right-arrow, typecasting.
%toke([ $-, $> |T], Cursor) ->
%   {#token{
%       type         = arrow,
%       start_cursor = Cursor,
%       end_cursor   = ?ADV_INDEX(Cursor, 2)
%      }, T};


% Single-character tokens.
toke([H|T], Cursor) ->
   Type_t = case H of
               %$.  -> dot;
               %$,  -> comma;
               $:  -> colon;
               $;  -> semi;
               %$$  -> dollar;
               %$@  -> at_symbol;

               %$(  -> left_paren;
               %$)  -> right_paren;

               %${  -> left_brace;
               %$}  -> right_brace;

               %$[  -> left_bracket;
               %$]  -> right_bracket;
               _   -> error(invalid_char)
            end,
   {#token{
       type         = Type_t,
       start_cursor = ?ADV_INDEX(Cursor),
       end_cursor   = ?ADV_INDEX(Cursor)
      }, T}.


strip_comment([ $\n |T], Cursor) -> toke(T, ?ADV_LINE(Cursor));
strip_comment([   _ |T], Cursor) -> strip_comment(T, ?ADV_INDEX(Cursor)).


identifier([H|T], Cursor) ->
   identifier(T, Cursor, Cursor, [H]).

identifier([H|T], Start, End, Text) when ?IS_WORDCHAR(H) ->
   identifier(T, Start, ?ADV_INDEX(End), [H|Text]);
identifier(Rest, Start, End, Text) ->
   {#token{
       type         = identifier,
       value        = lists:reverse(Text),
       start_cursor = Start,
       end_cursor   = End
      }, Rest}.


number([H|T], Cursor) ->
   number(T, Cursor, Cursor, [H]).

number([H|T], Start, End, Acc) when ?IS_NUMBER(H) ->
   number(T, Start, ?ADV_INDEX(End), [H|Acc]);
number(Rest, Start, End, Acc) ->
   {Integer, _} = string:to_integer(lists:reverse(Acc)),
   {#token{
       type         = number,
       value        = Integer,
       start_cursor = Start,
       end_cursor   = End
      }, Rest}.


%path(Li, Cursor) ->
%   path(Li, Cursor, Cursor, []).
%
%path([], _, _, _) ->
%   error(unterminated_path);
%path([ $' |T], Start, End, Text) ->
%   {#token{
%       type         = path,
%       value        = lists:reverse(Text),
%       start_cursor = Start,
%       end_cursor   = ?ADV_INDEX(End)
%      }, T};
%path([ $\\, $' |T], Start, End, Text) ->
%   path(T, Start, ?ADV_INDEX(End), [$' |Text]);
%path([ H |T], Start, End, Text) ->
%   path(T, Start, ?ADV_INDEX(End), [H |Text]).
%
%
%string(Li, Cursor) ->
%   string(Li, Cursor, Cursor, []).
%
%string([], _, _, _) ->
%   error(unterminated_string);
%string([ $" |T], Start, End, Text) ->
%   {#token{
%       type         = string,
%       value        = lists:reverse(Text),
%       start_cursor = Start,
%       end_cursor   = ?ADV_INDEX(End)
%      }, T};
%string([ $\\, $" |T], Start, End, Text) ->
%   string(T, Start, ?ADV_INDEX(End, 2), [$" |Text]);
%string([ H |T], Start, End, Text) ->
%   string(T, Start, ?ADV_INDEX(End), [H |Text]).
