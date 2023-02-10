-module(parser).
-export([ parse/0 ]).
-include("ast.hrl").
-include("token.hrl").


parse() ->
   parse(scanner:scan(), []).

parse([H], Decls) when H#token.type =:= eof ->
   lists:reverse(Decls);
parse(Ts, Decls) ->
   { D, Rest } = decl(Ts),
   parse(Rest, [D|Decls]).


munch(_, [], Msg) ->
   io:format("~s~n", [Msg]),
   exit(1);
munch(Exp, [H|T], Msg) ->
   if Exp#token.type =:= H -> H;
      true -> munch(Exp, T, Msg)
   end.


decl([Id|T]) ->
   % io:format("decl: ~p~n", [Id]),
   _ = munch(Id, [identifier], "decl must begin with ident"),

   [H|T1] = T,
   _ = munch(H, [colon], "expecting `:' after ident"),

   { Expr, Rest } = expr(T1),
   {#decl{
       name         = Id#token.value,
       expr         = Expr,
       start_cursor = Id#token.start_cursor,
       end_cursor   = Expr#expr.end_cursor
      }
    ,Rest}.


expr([H|T]) ->
   _ = munch(H, [number], "exprs are only numbers for now"),
   
   [H1|T1] = T,
   _ = munch(H1, [semi], "expects `;' after expr"),
   {#expr{
        kind         = number,
        lhs          = H#token.value,
        start_cursor = H#token.start_cursor,
        end_cursor   = H#token.end_cursor
      }
    ,T1}.
