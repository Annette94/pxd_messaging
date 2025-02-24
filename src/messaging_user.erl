%%%-------------------------------------------------------------------
%%% @doc Messaging User Interface Module
%%%
%%% This module provides the main interface for user operations in the messaging system.
%%% It handles user creation, session management, and interaction with messaging apps.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging_user).

-export([
    create_user/2,
    create_user_session/4,
    interact/2,
    show_state/1,
    show_history/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Creates a new user with the given username and password.
-spec create_user(Username :: string(), Password :: string()) ->
    {ok, user_created} | {error, user_already_exists}.
create_user(Username, Password) ->
    messaging_db:save_user(Username, Password).

%% @doc Creates a new session for an authenticated user.
-spec create_user_session(Username :: string(),
                         Password :: string(),
                         AppName :: string(),
                         StartStateId :: string()) ->
    {ok, binary()} | {error, invalid_credentials}.
create_user_session(Username, Password, AppName, StartStateId) ->
    case messaging_db:verify_user(Username, Password) of
        {atomic, true} ->
            SessionId = crypto:strong_rand_bytes(16),
            {ok, Pid} = messaging_session_sup:start_session(SessionId, Username, AppName, StartStateId),
            ok = messaging_session_registry:register_session(SessionId, Username, Pid),
            messaging_db:create_session(SessionId, Username),
            
            % Display intro message immediately after creating the session
            messaging_session:show_intro(SessionId),
            
            {ok, SessionId};
        {atomic, false} ->
            {error, invalid_credentials}
    end.

%% @doc Sends a message in the context of a user's session.
-spec interact(SessionId :: binary(), Message :: term()) ->
    ok | {error, term()}.
interact(SessionId, Message) ->
    messaging_session:interact(SessionId, Message).

%% @doc Shows the current state of a user's session.
-spec show_state(SessionId :: binary()) ->
    {ok, string()} | {error, term()}.
show_state(SessionId) ->
    messaging_session:show_state(SessionId).

%% @doc Shows the interaction history of a user's session.
-spec show_history(SessionId :: binary()) ->
    {ok, [{string(), term()}]} | {error, term()}.
show_history(SessionId) ->
    messaging_session:show_history(SessionId).
