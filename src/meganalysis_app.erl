-module(meganalysis_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    {ok, Pid} = meganalysis_sup:start_link(),
    meganalysis_log_info:add_handler(),
    {ok, Pid}.

stop(_State) ->
    ok.
