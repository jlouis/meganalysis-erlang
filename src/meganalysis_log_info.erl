%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <jesper.louis.andersen@gmail.com>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc An Info-report logger through the error handler
%%% @end
%%% Created :  8 Apr 2011 by Jesper Louis andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(meganalysis_log_info).

-behaviour(gen_event).

%% API
-export([add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @doc add the event handler
add_handler() ->
    ok = meganalysis_event:add_handler(?MODULE).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_event(Term, State) ->
    error_logger:info_report([{got_event, Term}]),
    {ok, State}.

%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
