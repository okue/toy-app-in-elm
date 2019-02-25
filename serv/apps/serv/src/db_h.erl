-module(db_h).
-export([select/1]).

select(_Query) ->
    Res = [ 1, 2, 3, 4, 5 ],
    {ok, Res}.
