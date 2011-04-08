%% @doc API for the meganalysis application
%% @end
-module(meganalysis).

-export([total/0, cut/0]).

total() ->
    meganalysis_process:total().

cut() ->
    meganalysis_process:cut().

