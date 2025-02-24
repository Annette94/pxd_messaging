%%%-------------------------------------------------------------------
%%% @doc Messaging Session Supervisor
%%%
%%% This module implements a supervisor for messaging sessions.
%%% It manages the lifecycle of individual messaging sessions 
%%% and provides functionality to start new sessions.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging_session_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_session/4]).

%% Type definitions
-type sup_flags() :: #{strategy => simple_one_for_one,
                      intensity => non_neg_integer(),
                      period => pos_integer()}.
-type child_spec() :: #{id := term(),
                       start := {module(), atom(), list()},
                       restart := temporary | permanent | transient}.

%% @doc Starts the session supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes the supervisor with simple_one_for_one strategy.
-spec init([]) -> {ok, {sup_flags(), [child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [#{id => messaging_session,
                    start => {messaging_session, start_link, []},
                    restart => temporary}],
    {ok, {SupFlags, ChildSpecs}}.

%% @doc Starts a new messaging session under this supervisor.
-spec start_session(SessionId :: binary(),
                   Username :: string(),
                   AppName :: string(),
                   StartStateId :: string()) ->
    {ok, pid()} | {error, term()}.
start_session(SessionId, Username, AppName, StartStateId) ->
    supervisor:start_child(?MODULE, [SessionId, Username, AppName, StartStateId]).
