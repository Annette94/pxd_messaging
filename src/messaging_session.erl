%%%-------------------------------------------------------------------
%%% @doc Messaging Session Server
%%%
%%% This module implements a gen_server for managing individual messaging sessions.
%%% It handles message interactions, maintains session state and history, and
%%% provides functionality for state restoration and persistence.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------
-module(messaging_session).
-behaviour(gen_server).

-export([start_link/4, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([interact/2, show_state/1, show_history/1, show_intro/1, get_session_timeout/0]).

-record(state, {
    app_name,
    current_state,
    history = [],
    session_id,
    username,
    last_activity
}).

-define(DEFAULT_TIMEOUT, 300).

%% Type definitions
-type from() :: {pid(), term()}.
-type session_state() :: #state{}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts a new messaging session with the given parameters.
-spec start_link(SessionId :: binary(),
                Username :: string(),
                AppName :: string(),
                StartStateId :: string()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Username, AppName, StartStateId) ->
    gen_server:start_link(?MODULE, [SessionId, Username, AppName, StartStateId], []).

%% @doc Sends an interaction message to a specific session.
-spec interact(SessionId :: binary(), Message :: term()) -> ok | {error, term()}.
interact(SessionId, Message) ->
    case messaging_session_registry:get_session_pid(SessionId) of
        {ok, Pid} -> gen_server:call(Pid, {interact, SessionId, Message});
        Error -> Error
    end.

%% @doc Retrieves the current state of a session.
-spec show_state(SessionId :: binary()) -> {ok, string()} | {error, term()}.
show_state(SessionId) ->
    case messaging_session_registry:get_session_pid(SessionId) of
        {ok, Pid} -> gen_server:call(Pid, show_state);
        Error -> Error
    end.

%% @doc Retrieves the interaction history of a session.
-spec show_history(SessionId :: binary()) -> {ok, [{string(), term()}]} | {error, term()}.
show_history(SessionId) ->
    case messaging_session_registry:get_session_pid(SessionId) of
        {ok, Pid} -> gen_server:call(Pid, show_history);
        Error -> Error
    end.

%% @doc Shows the introduction message for the current state.
-spec show_intro(SessionId :: binary()) -> ok | {error, term()}.
show_intro(SessionId) ->
    case messaging_session_registry:get_session_pid(SessionId) of
        {ok, Pid} -> gen_server:call(Pid, show_intro);
        Error -> Error
    end.

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

%% @doc Initializes the server state, restoring previous state if available.
-spec init([term()]) -> {ok, session_state()}.
init([SessionId, Username, AppName, StartStateId]) ->
    % Try to restore previous state on startup
    State = case messaging_db:load_state(AppName, Username) of
        {atomic, {ok, SavedState}} ->
            case SavedState#state.session_id of
                SessionId -> 
                    SavedState;  % Keep the same state if session is unchanged
                _ ->
                    % Update session_id and last_activity if it's a new session
                    SavedState#state{
                        session_id = SessionId,
                        current_state = StartStateId,
                        last_activity = erlang:system_time(second)
                    }
            end;
        _ ->
            #state{
                session_id = SessionId,
                username = Username,
                app_name = AppName,
                current_state = StartStateId,
                last_activity = erlang:system_time(second)
            }
    end,

    % Also try to restore history
    History = case messaging_db:load_history(AppName, Username) of
        {atomic, {ok, SavedHistory}} -> SavedHistory;
        _ -> []
    end,
    {ok, State#state{history = History}}.

%% @doc Handles synchronous calls to the server.
-spec handle_call(term(), from(), session_state()) ->
    {reply, term(), session_state()} | {reply, {error, term()}, session_state()}.
handle_call(show_intro, _From, State) ->
    Result = messaging:get_current_message(State#state.app_name, State#state.current_state),
    NewState = update_activity_timestamp(State),
    case Result of
        {ok, _} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, NewState}
    end;

handle_call({interact, SessionId, Message}, _From, State) ->
    NewState = update_activity_timestamp(State),
    case validate_session(SessionId, NewState) of
        ok ->
            case check_session_timeout(NewState) of
                ok ->
                    InteractState = handle_interaction(Message, NewState),
                    ok = save_current_state(InteractState),
                    {reply, ok, InteractState};
                {error, Reason} ->
                    {reply, {error, Reason}, NewState}
            end;
        {error, invalid_session} ->
            {reply, {error, unauthorized}, NewState}
    end;

handle_call(show_state, _From, State) ->
    NewState = update_activity_timestamp(State),
    {reply, {ok, NewState#state.current_state}, NewState};

handle_call(show_history, _From, State) ->
    NewState = update_activity_timestamp(State),
    {reply, {ok, NewState#state.history}, NewState}.

%% @doc Handles asynchronous messages (currently unused).
-spec handle_cast(term(), session_state()) -> {noreply, session_state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handles system messages, including timeout.
-spec handle_info(timeout | term(), session_state()) -> {stop, normal, session_state()}.
handle_info(timeout, State) ->
    {stop, normal, State}.

%% @doc Handles server termination, saving state before shutdown.
-spec terminate(term(), session_state()) -> ok.
terminate(_Reason, State) ->
    % Save state before termination
    save_current_state(State),
    messaging_session_registry:remove_session(State#state.session_id),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Saves the current session state to persistent storage.
-spec save_current_state(session_state()) -> ok.
save_current_state(State) ->
    messaging_db:save_state(State#state.app_name, State#state.username, State),
    messaging_db:save_history(State#state.app_name, State#state.username, State#state.history),
    ok.

-spec get_session_timeout() -> non_neg_integer().
get_session_timeout() ->
    application:get_env(messaging, session_timeout, ?DEFAULT_TIMEOUT).

%% @doc Checks if the session has timed out.
-spec check_session_timeout(session_state()) -> ok | {error, session_timeout}.
check_session_timeout(State) ->
    Now = erlang:system_time(second),
    Timeout = get_session_timeout(),
    case Now - State#state.last_activity > Timeout of
        true -> {error, session_timeout};
        false -> ok
    end.

%% @doc Updates the last activity timestamp in the session state.
-spec update_activity_timestamp(session_state()) -> session_state().
update_activity_timestamp(State) ->
    State#state{last_activity = erlang:system_time(second)}.

%% @doc Processes an interaction message and updates state accordingly.
-spec handle_interaction(term(), session_state()) -> session_state().
handle_interaction(Message, State) ->
    case messaging:handle_message(State#state.app_name, State#state.current_state, State#state.history, Message) of
        ok -> State;
        {ok, NextState, NextMsg, _CurrentMsg} ->
            case State#state.app_name of
                agri_survey_app -> ok;
                _ ->
                    % Print the next message
                    io:format("~p > ~s~n", [State#state.app_name, NextMsg])
            end,
            
            % Update history with current state and message
            NewHistory = [{State#state.current_state, Message} | State#state.history],
            
            % Return updated state
            State#state{
                current_state = NextState,
                history = NewHistory
            };
            
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            % On error, maintain current state but update last activity
            State
    end.

%% @doc Validates that the session ID matches the current state.
-spec validate_session(binary(), session_state()) -> ok | {error, invalid_session}.
validate_session(SessionId, State) ->
    case SessionId == State#state.session_id of
        true -> ok;
        false -> {error, invalid_session}
    end.
