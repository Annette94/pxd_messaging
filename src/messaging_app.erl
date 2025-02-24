%%%-------------------------------------------------------------------
%%% @doc Messaging Application Module
%%%
%%% This module implements the main application behavior for the messaging system.
%%% It handles application startup, shutdown
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Starts the messaging application and initializes necessary components.
-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ok = messaging_db:init(),
    {ok, Pid} = messaging_sup:start_link(),
    ok = messaging:init(),
    {ok, Pid}.

%% @doc Stops the messaging application.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
