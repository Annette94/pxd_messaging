%%%-------------------------------------------------------------------
%%% @doc Messaging Session Registry
%%%
%%% This module implements a session registry using gen_server behavior.
%%% It maintains a mapping of session IDs to their corresponding user and process
%%% information, providing functionality for session registration, lookup,
%%% and removal.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging_session_registry).
-behaviour(gen_server).

-export([start_link/0, register_session/3, get_session/1, get_session_pid/1,
         remove_session/1, init/1, handle_call/3, handle_cast/2]).

-record(state, {sessions = #{}}).

%% Type definitions
-type session_id() :: binary().
-type username() :: string().
-type session_info() :: {username(), pid()}.
-type from() :: {pid(), term()}.

%%====================================================================
%% API functions
%%====================================================================
%% @doc Starts the session registry server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Registers a new session with the given SessionId, Username and Pid.
-spec register_session(session_id(), username(), pid()) -> ok.
register_session(SessionId, Username, Pid) ->
    gen_server:call(?MODULE, {register, SessionId, Username, Pid}).

%% @doc Retrieves session information for the given SessionId.
-spec get_session(session_id()) -> {ok, session_info()} | {error, not_found}.
get_session(SessionId) ->
    gen_server:call(?MODULE, {get, SessionId}).

%% @doc Retrieves the process ID associated with the given SessionId.
-spec get_session_pid(session_id()) -> {ok, pid()} | {error, not_found}.
get_session_pid(SessionId) ->
    gen_server:call(?MODULE, {get_pid, SessionId}).

%% @doc Removes a session from the registry.
-spec remove_session(session_id()) -> ok.
remove_session(SessionId) ->
    gen_server:cast(?MODULE, {remove, SessionId}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================
%% @doc Initializes the server state.
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

%% @doc Handles synchronous calls to the server.
-spec handle_call(
    {register, session_id(), username(), pid()} |
    {get_pid, session_id()} |
    {get, session_id()},
    from(),
    #state{}) ->
    {reply, term(), #state{}}.
handle_call({register, SessionId, Username, Pid}, _From, State) ->
    monitor(process, Pid),
    NewSessions = maps:put(SessionId, {Username, Pid}, State#state.sessions),
    {reply, ok, State#state{sessions = NewSessions}};

handle_call({get_pid, SessionId}, _From, State) ->
    Reply = case maps:find(SessionId, State#state.sessions) of
        {ok, {_Username, Pid}} -> {ok, Pid};
        error -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get, SessionId}, _From, State) ->
    Reply = case maps:find(SessionId, State#state.sessions) of
        {ok, Value = {_Username, _Pid}} -> 
            {ok, Value};
        error -> {error, not_found}
    end,
    {reply, Reply, State}.

%% @doc Handles asynchronous casts to the server.
-spec handle_cast({remove, session_id()}, #state{}) ->
    {noreply, #state{}}.
handle_cast({remove, SessionId}, State) ->
    NewSessions = maps:remove(SessionId, State#state.sessions),
    {noreply, State#state{sessions = NewSessions}}.
