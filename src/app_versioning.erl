%%%-------------------------------------------------------------------
%%% @doc App Versioning Module
%%%
%%% This module implements a versioning system for applications, allowing storage
%%% and retrieval of different versions of app data. It supports creating new
%%% versions, retrieving specific or latest versions, and managing version-specific
%%% data using Mnesia database.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(app_versioning).

-export([
    create_version/2,
    get_version/2,
    get_latest_version/1,
    save_app_data/3,
    get_app_data/2,
    get_app_data/3,
    list_app_versions/1
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("records.hrl").

%%====================================================================
%% Internal functions for table selection
%%====================================================================

%% @private
%% @doc Returns the correct table name based on environment
get_table_name(TableName) ->
    case application:get_env(messaging, test_mode, false) of
        true ->
            case TableName of
                app_version -> app_version_test;
                app_data -> app_data_test
            end;
        false ->
            TableName
    end.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Creates a new version for an app with given metadata.
-spec create_version(string(), map()) -> {ok, string()}.
create_version(AppName, Metadata) ->
    VersionId = generate_id(),
    Timestamp = os:system_time(seconds),
    
    Version = #app_version{
        app_name = AppName,
        version_id = VersionId,
        timestamp = Timestamp,
        metadata = Metadata
    },
    
    Table = get_table_name(app_version),
    mnesia:dirty_write(Table, Version),
    {ok, VersionId}.

%% @doc Retrieves the latest version for the specified app.
-spec get_latest_version(string()) -> {ok, #app_version{}} | {error, no_versions}.
get_latest_version(AppName) ->
    Table = get_table_name(app_version),
    Versions = mnesia:dirty_select(Table, [
        {#app_version{app_name = AppName, _ = '_'}, [], ['$_']}
    ]),
    
    case Versions of
        [] -> {error, no_versions};
        _ -> 
            SortedVersions = lists:sort(
                fun(#app_version{timestamp = T1}, #app_version{timestamp = T2}) ->
                    T1 > T2
                end,
                Versions
            ),
            {ok, hd(SortedVersions)}
    end.

%% @doc Gets a specific version of an app.
-spec get_version(string(), string()) -> {ok, #app_version{}} | {error, not_found}.
get_version(AppName, VersionId) ->
    Table = get_table_name(app_version),
    MatchSpec = [
        {#app_version{app_name = AppName, version_id = VersionId, _ = '_'}, [], ['$_']}
    ],
    case mnesia:dirty_select(Table, MatchSpec) of
        [Version | _] -> {ok, Version};
        [] -> {error, not_found}
    end.

%% @doc Saves data for a specific app version.
-spec save_app_data(string(), string(), term()) -> ok.
save_app_data(AppName, VersionId, Data) ->
    AppData = #app_data{
        app_name = AppName,
        version_id = VersionId,
        data = Data
    },
    Table = get_table_name(app_data),
    mnesia:dirty_write(Table, AppData),
    ok.

%% @doc Gets app data for a specific version, returns default if not found.
-spec get_app_data(string(), string(), term()) -> {ok, term()}.
get_app_data(AppName, VersionId, Default) ->
    Table = get_table_name(app_data),
    MatchSpec = [
        {#app_data{app_name = AppName, version_id = VersionId, data = '$1', _ = '_'},
         [],
         ['$1']}
    ],
    case mnesia:dirty_select(Table, MatchSpec) of
        [Data | _] -> {ok, Data};
        [] -> {ok, Default}
    end.

%% @doc Gets app data for the latest version, returns default if not found.
-spec get_app_data(string(), term()) -> {ok, term()}.
get_app_data(AppName, Default) ->
    case get_latest_version(AppName) of
        {ok, #app_version{version_id = VersionId}} ->
            get_app_data(AppName, VersionId, Default);
        {error, _} ->
            {ok, Default}
    end.

%% @doc Lists all versions for an app, sorted by timestamp descending.
-spec list_app_versions(string()) -> [#app_version{}].
list_app_versions(AppName) ->
    Table = get_table_name(app_version),
    Versions = mnesia:dirty_select(Table, [
        {#app_version{app_name = AppName, _ = '_'}, [], ['$_']}
    ]),
    
    lists:sort(
        fun(#app_version{timestamp = T1}, #app_version{timestamp = T2}) ->
            T1 > T2
        end,
        Versions
    ).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Generates a unique ID for versioning.
-spec generate_id() -> string().
generate_id() ->
    <<ID:160>> = crypto:strong_rand_bytes(20),
    lists:flatten(io_lib:format("~40.16.0b", [ID])).
