-module(app_versioning_tests).
-include_lib("eunit/include/eunit.hrl").

-record(app_version, {app_name, version_id, timestamp, metadata}).

%% Test Fixtures
app_versioning_test_() ->
    {setup,
     fun() -> % Setup - runs once before all tests
         application:set_env(messaging, test_mode, true),
         mnesia:stop(),
         ok = messaging_db:init(),
         ok = mnesia:wait_for_tables([app_version_test, app_data_test], 5000),
         reset_tables()
     end,
     fun(_) -> % Cleanup - runs once after all tests
         application:set_env(messaging, test_mode, false),
         reset_tables()
     end,
     % Actual test functions
     {inorder, [
         fun version_creation/0,
         fun latest_version/0,
         fun app_data_storage/0,
         fun version_listing/0,
         fun default_value/0,
         fun multiple_apps/0
     ]}
    }.

reset_tables() ->
    Tables = [app_version_test, app_data_test],
    [clear_table(T) || T <- Tables],
    ok.

clear_table(Table) ->
    case mnesia:clear_table(Table) of
        {atomic, ok} -> ok;
        {aborted, {no_exists, Table}} -> ok;
        Error -> Error
    end.

%% Individual Tests
version_creation() ->
    % Create a version with metadata
    Metadata = #{description => "Initial version"},
    {ok, VersionId1} = app_versioning:create_version("test_app", Metadata),
    ?assertMatch({ok, #app_version{
        app_name = "test_app",
        version_id = VersionId1,
        metadata = Metadata
    }}, app_versioning:get_version("test_app", VersionId1)),
    
    % Create another version
    {ok, VersionId2} = app_versioning:create_version("test_app", #{description => "Second version"}),
    ?assertNotEqual(VersionId1, VersionId2).

latest_version() ->
    % Create versions with delay to ensure different timestamps
    {ok, _V1} = app_versioning:create_version("time_app", #{version => "1.0"}),
    timer:sleep(1000),
    {ok, V2} = app_versioning:create_version("time_app", #{version => "2.0"}),
    
    % Get latest version
    {ok, LatestVersion} = app_versioning:get_latest_version("time_app"),
    ?assertEqual(V2, LatestVersion#app_version.version_id),
    
    % Test non-existent app
    ?assertEqual({error, no_versions}, app_versioning:get_latest_version("nonexistent_app")).

app_data_storage() ->
    % Create version and store data
    {ok, VersionId} = app_versioning:create_version("data_app", #{description => "Test data"}),
    TestData = #{key => "value"},
    
    % Save and retrieve data
    ok = app_versioning:save_app_data("data_app", VersionId, TestData),
    {ok, RetrievedData} = app_versioning:get_app_data("data_app", VersionId, undefined),
    ?assertEqual(TestData, RetrievedData),
    
    % Test with non-existent version
    {ok, DefaultValue} = app_versioning:get_app_data("data_app", "nonexistent_version", default_value),
    ?assertEqual(default_value, DefaultValue).

version_listing() ->
    % Create multiple versions
    {ok, _} = app_versioning:create_version("list_app", #{version => "1.0"}),
    timer:sleep(1000),
    {ok, _} = app_versioning:create_version("list_app", #{version => "2.0"}),
    
    % Get version list
    Versions = app_versioning:list_app_versions("list_app"),
    ?assertEqual(2, length(Versions)),
    
    % Verify order (newest first)
    [First, Second] = Versions,
    ?assert(First#app_version.timestamp > Second#app_version.timestamp).

default_value() ->
    DefaultValue = #{default => true},
    
    % Test with no versions
    {ok, Default1} = app_versioning:get_app_data("new_app", DefaultValue),
    ?assertEqual(DefaultValue, Default1),
    
    % Test with version but no data
    {ok, VersionId} = app_versioning:create_version("new_app", #{}),
    {ok, Default2} = app_versioning:get_app_data("new_app", VersionId, DefaultValue),
    ?assertEqual(DefaultValue, Default2).

multiple_apps() ->
    % Create versions for different apps
    {ok, V1} = app_versioning:create_version("app1", #{version => "1.0"}),
    {ok, V2} = app_versioning:create_version("app2", #{version => "1.0"}),
    
    % Store different data
    Data1 = #{app => "1"},
    Data2 = #{app => "2"},
    ok = app_versioning:save_app_data("app1", V1, Data1),
    ok = app_versioning:save_app_data("app2", V2, Data2),
    
    % Verify data isolation
    {ok, Retrieved1} = app_versioning:get_app_data("app1", V1, undefined),
    {ok, Retrieved2} = app_versioning:get_app_data("app2", V2, undefined),
    ?assertEqual(Data1, Retrieved1),
    ?assertEqual(Data2, Retrieved2).
