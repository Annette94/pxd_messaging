%%%-------------------------------------------------------------------
%%% @doc Messaging Top Level Supervisor
%%%
%%% This module implements the top-level supervisor for the messaging system.
%%% It supervises the session supervisor and session registry processes using
%%% a one_for_one strategy.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% Type definitions
-type sup_flags() :: #{strategy => one_for_one,
                      intensity => non_neg_integer(),
                      period => pos_integer()}.
-type child_spec() :: #{id := term(),
                       start := {module(), atom(), list()},
                       type := supervisor | worker}.

%% @doc Starts the top level supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes the supervisor with one_for_one strategy and child specifications.
-spec init([]) -> {ok, {sup_flags(), [child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [
        #{id => messaging_session_sup,
          start => {messaging_session_sup, start_link, []},
          type => supervisor},
        #{id => messaging_session_registry,
          start => {messaging_session_registry, start_link, []},
          type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.
