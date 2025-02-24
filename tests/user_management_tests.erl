-module(user_management_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test generator
user_management_test_() ->
    {setup,
     fun() -> % Setup - runs once before all tests
         application:set_env(messaging, test_mode, true),
         mnesia:stop(),
         ok = messaging_db:init(),
         ok = mnesia:wait_for_tables([user_test, app_state_test, 
                                    app_history_test, session_test], 5000),
         reset_tables()
     end,
     fun(_) -> % Cleanup - runs once after all tests
         application:set_env(messaging, test_mode, false),
         reset_tables()
     end,
     % Actual test functions
     {inorder, [
         fun user_creation/0,
         fun user_verification/0,
         fun session_management/0,
         fun invalid_login/0,
         fun session_expiration/0
     ]}
    }.

reset_tables() ->
    Tables = [user_test, app_state_test, app_history_test, session_test],
    [clear_table(T) || T <- Tables],
    ok.

clear_table(Table) ->
    case mnesia:clear_table(Table) of
        {atomic, ok} -> ok;
        {aborted, {no_exists, Table}} -> ok;
        Error -> Error
    end.

%% Individual Tests
user_creation() ->
    io:format("Starting user_creation test~n"),
    
    Result1 = messaging_db:save_user("test_user", "password"),
    io:format("First save result: ~p~n", [Result1]),
    ?assertEqual({ok, user_created}, Result1),
    
    Result2 = messaging_db:save_user("test_user", "password"),
    io:format("Second save result: ~p~n", [Result2]),
    ?assertEqual({error, user_already_exists}, Result2).

user_verification() ->
    % Create a test user
    {ok, user_created} = messaging_db:save_user("auth_test_user", "correct_password"),
    
    % Test successful verification
    {atomic, true} = messaging_db:verify_user("auth_test_user", "correct_password"),
    
    % Test failed verification with wrong password
    {atomic, false} = messaging_db:verify_user("auth_test_user", "wrong_password"),
    
    % Test non-existent user
    {atomic, false} = messaging_db:verify_user("non_existent_user", "any_password").

session_management() ->
    % Create a test user
    {ok, user_created} = messaging_db:save_user("session_user", "password"),
    
    % Create a session
    SessionId = generate_session_id(),
    {atomic, ok} = messaging_db:create_session(SessionId, "session_user"),
    
    % Verify valid session
    {atomic, {ok, "session_user"}} = messaging_db:verify_session(SessionId, 3600).

invalid_login() ->
    % Test login with empty credentials
    {atomic, false} = messaging_db:verify_user("", ""),
    
    % Test login with special characters
    {ok, user_created} = messaging_db:save_user("special@user", "pass#word"),
    {atomic, true} = messaging_db:verify_user("special@user", "pass#word").

session_expiration() ->
    % Create a test user and session
    {ok, user_created} = messaging_db:save_user("expiry_test_user", "password"),
    SessionId = generate_session_id(),
    {atomic, ok} = messaging_db:create_session(SessionId, "expiry_test_user"),
    
    % Test with very short expiry time (1 second)
    timer:sleep(2000),  % Wait for more than 1 second
    {atomic, {error, session_expired}} = messaging_db:verify_session(SessionId, 1).

%% Helper functions
generate_session_id() ->
    base64:encode(crypto:strong_rand_bytes(32)).
