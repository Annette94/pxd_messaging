%%%-------------------------------------------------------------------
%%% @doc Messaging Database Module
%%%
%%% This module handles all database operations for the messaging system using Mnesia.
%%% It manages user, session and application state related DB calls.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @doc Messaging Database Module
%%%
%%% This module handles all database operations for the messaging system using Mnesia.
%%% It manages user, session and application state related DB calls.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging_db).
-export([init/0, save_state/3, load_state/2, save_history/3, load_history/2,
         save_user/2, verify_user/2, create_session/2, verify_session/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("records.hrl").

%% @doc Initializes the database schema and creates required tables.
-spec init() -> ok.
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_tables().

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Gets the appropriate table name based on environment
get_table_name(TableName) ->
    case application:get_env(messaging, test_mode, false) of
        true -> list_to_atom(atom_to_list(TableName) ++ "_test");
        false -> TableName
    end.

%% @doc Creates tables if they don't exist.
-spec create_tables() -> ok.
create_tables() ->
    % Get table names based on environment
    AppStateTable = get_table_name(app_state),
    AppHistoryTable = get_table_name(app_history),
    UserTable = get_table_name(user),
    SessionTable = get_table_name(session),
    AppVersionTable = get_table_name(app_version),
    AppDataTable = get_table_name(app_data),

    % Create all necessary tables
    create_table_if_not_exists(AppStateTable, app_state, record_info(fields, app_state)),
    create_table_if_not_exists(AppHistoryTable, app_history, record_info(fields, app_history)),
    create_table_if_not_exists(UserTable, user, record_info(fields, user)),
    create_table_if_not_exists(SessionTable, session, record_info(fields, session)),
    create_table_if_not_exists(AppVersionTable, app_version, record_info(fields, app_version), [{type, bag}]),
    create_table_if_not_exists(AppDataTable, app_data, record_info(fields, app_data), [{type, bag}]),
    ok.

%% Rest of the functions need to be updated to use get_table_name/1
save_state(AppName, UserName, State) ->
    Table = get_table_name(app_state),
    F = fun() ->
        mnesia:write(Table, #app_state{app_name = AppName, username = UserName, state = State}, write)
    end,
    mnesia:transaction(F).

load_state(AppName, UserName) ->
    Table = get_table_name(app_state),
    F = fun() ->
        case mnesia:match_object(Table, #app_state{app_name = AppName, username = UserName, state = '_'}, read) of
            [#app_state{state = State}] -> {ok, State};
            [] -> {error, not_found}
        end
    end,
    mnesia:transaction(F).

save_history(AppName, UserName, History) ->
    Table = get_table_name(app_history),
    F = fun() ->
        mnesia:write(Table, #app_history{app_name = AppName, username = UserName, history = History}, write)
    end,
    mnesia:transaction(F).

load_history(AppName, UserName) ->
    Table = get_table_name(app_history),
    F = fun() ->
        case mnesia:match_object(Table, #app_history{app_name = AppName, username = UserName, history = '_'}, read) of
            [#app_history{history = History}] -> {ok, History};
            [] -> {error, not_found}
        end
    end,
    mnesia:transaction(F).

save_user(Username, Password) ->
    Table = get_table_name(user),
    case user_exists(Username) of
        true -> {error, user_already_exists};
        false ->
            Salt = crypto:strong_rand_bytes(16),
            Hash = hash_password(Password, Salt),
            F = fun() ->
                mnesia:write(Table, #user{username = Username,
                                        password_hash = Hash,
                                        salt = Salt}, write)
            end,
            mnesia:transaction(F),
            {ok, user_created}
    end.

verify_user(Username, Password) ->
    Table = get_table_name(user),
    F = fun() ->
        case mnesia:read(Table, Username, read) of
            [#user{password_hash = Hash, salt = Salt}] ->
                hash_password(Password, Salt) =:= Hash;
            [] ->
                false
        end
    end,
    mnesia:transaction(F).

create_session(SessionId, Username) ->
    Table = get_table_name(session),
    F = fun() ->
        mnesia:write(Table, #session{session_id = SessionId,
                                   username = Username,
                                   timestamp = erlang:system_time(second)}, write)
    end,
    mnesia:transaction(F).

verify_session(SessionId, MaxAge) ->
    Table = get_table_name(session),
    Now = erlang:system_time(second),
    F = fun() ->
        case mnesia:read(Table, SessionId, read) of
            [#session{username = Username, timestamp = Timestamp}] ->
                case Now - Timestamp =< MaxAge of
                    true ->
                        mnesia:write(Table, #session{session_id = SessionId,
                                                   username = Username,
                                                   timestamp = Now}, write),
                        {ok, Username};
                    false ->
                        mnesia:delete({Table, SessionId}),
                        {error, session_expired}
                end;
            [] ->
                {error, invalid_session}
        end
    end,
    mnesia:transaction(F).

%% Helper functions remain unchanged
create_table_if_not_exists(TableName, RecordName, Attributes) ->
    create_table_if_not_exists(TableName, RecordName, Attributes, []).

create_table_if_not_exists(TableName, RecordName, Attributes, ExtraOptions) ->
    Options = [
        {record_name, RecordName},
        {attributes, Attributes},
        {disc_copies, [node()]}
        | ExtraOptions
    ],
    case mnesia:create_table(TableName, Options) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, TableName}} -> ok;
        Error -> Error
    end.

hash_password(Password, Salt) ->
    crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>).

user_exists(Username) ->
    Table = get_table_name(user),
    case mnesia:transaction(fun() -> mnesia:read(Table, Username, read) end) of
        {atomic, [_|_]} -> true;
        _ -> false
    end.
