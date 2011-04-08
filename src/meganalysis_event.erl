%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <jesper.louis.andersen@gmail.com>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc An Event Logger for 101companies
%%% @end
%%% Created :  8 Apr 2011 by Jesper Louis andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(meganalysis_event).

%% API
-export([start_link/0, add_handler/1]).

-export([notify/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @doc Create the event manager
%% @end
start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(HandlerMod) ->
    gen_event:add_handler(?SERVER, HandlerMod, []).

notify(Term) ->
    gen_event:notify(?SERVER, Term).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================





