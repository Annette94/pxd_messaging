%%%-------------------------------------------------------------------
%%% @doc Test cases for messaging module focusing on core functionalities
%%%-------------------------------------------------------------------
-module(messaging_tests).

-include_lib("eunit/include/eunit.hrl").

%% Include records used in tests
-record(app_state, {msg, logic}).
-record(question, {id, msg, opts = [], anchor = []}).

%%====================================================================
%% Test Fixtures
%%====================================================================

messaging_test_() ->
    {setup,
     fun() -> % Setup - runs once before all tests
         application:set_env(messaging, test_mode, true),
         mnesia:stop(),
         ok = messaging_db:init(),
         ok = mnesia:wait_for_tables([app_state_test, app_history_test], 5000),
         reset_tables(),
         ok = messaging:init()
     end,
     fun(_) -> % Cleanup - runs once after all tests
         application:set_env(messaging, test_mode, false),
         reset_tables()
     end,
     % Group tests by app type
     {foreach,
      fun() -> ok end,
      fun(_) -> reset_tables() end,
      [
       {"Example App Tests",
        {inorder, [fun test_example_app/0,
                  fun test_example_app_errors/0]}},
       {"Survey App Tests",
        {inorder, [fun test_survey_app/0,
                  fun test_survey_app_errors/0]}},
       {"Agri Survey App Tests",
        {inorder, [fun test_agri_survey_app/0,
                  fun test_agri_survey_app_errors/0]}},
       {"History Management Tests",
        fun test_history_management/0},
       {"Initial App Data Tests",
        fun test_initial_app_data/0}
      ]}}.

%%====================================================================
%% Helper Functions
%%====================================================================

reset_tables() ->
    Tables = [app_state_test, app_history_test],
    [clear_table(T) || T <- Tables],
    ok.

clear_table(Table) ->
    case mnesia:clear_table(Table) of
        {atomic, ok} -> ok;
        {aborted, {no_exists, Table}} -> ok;
        Error -> Error
    end.

%%====================================================================
%% Test Cases
%%====================================================================

%%====================================================================
%% Test Cases
%%====================================================================

test_example_app() ->
    % Test rice path
    {ok, rice1, Msg1, _} = messaging:handle_message(example_app, intro, [], "1"),
    ?assert(string:str(Msg1, "glaberrima") > 0),
    
    History1 = [{intro, "1"}],
    {ok, rice2, Msg2, _} = messaging:handle_message(example_app, rice1, History1, "next"),
    ?assert(string:str(Msg2, "cereal") > 0),
    
    History2 = [{rice1, "next"}, {intro, "1"}],
    {ok, rice3, Msg3, _} = messaging:handle_message(example_app, rice2, History2, "next"),
    ?assert(string:str(Msg3, "maize") > 0),
    
    % Test cotton path
    {ok, cotton1, Msg4, _} = messaging:handle_message(example_app, intro, [], "2"),
    ?assert(string:str(Msg4, "Cotton") > 0),
    
    History3 = [{intro, "2"}],
    {ok, cotton2, Msg5, _} = messaging:handle_message(example_app, cotton1, History3, "next"),
    ?assert(string:str(Msg5, "cellulose") > 0).

test_example_app_errors() ->
    % Test invalid input at intro
    Result1 = messaging:handle_message(example_app, intro, [], "3"),
    ?assertMatch({ok, intro, _, _}, Result1),
    
    % Test invalid state transition
    Result2 = messaging:handle_message(example_app, rice1, [], "invalid"),
    ?assertMatch({ok, rice2, _, _}, Result2),
    
    % Test non-existent state
    Result3 = messaging:handle_message(example_app, invalid_state, [], "1"),
    ?assertMatch({error, invalid_state}, Result3).

test_survey_app() ->
    % Test rice path with anchoring
    {ok, rice1, Msg1, _} = messaging:handle_message(survey_app, intro, [], "1"),
    ?assert(string:str(Msg1, "Rice") > 0),
    
    % Test rice subtopics
    History1 = [{intro, "1"}],
    {ok, rice2, _, _} = messaging:handle_message(survey_app, rice1, History1, "2"),
    ?assertMatch({ok, _}, messaging:get_current_message(survey_app, rice2)),
    
    % Test cotton path
    {ok, cotton1, Msg2, _} = messaging:handle_message(survey_app, intro, [], "2"),
    ?assert(string:str(Msg2, "Cotton") > 0),
    
    % Verify anchoring
    History2 = [{intro, "2"}],
    {ok, cotton2, _, _} = messaging:handle_message(survey_app, cotton1, History2, "next"),
    ?assertMatch({ok, _}, messaging:get_current_message(survey_app, cotton2)).

test_survey_app_errors() ->
    % Test invalid option
    Result1 = messaging:handle_message(survey_app, intro, [], "3"),
    ?assertMatch({error, invalid_option}, Result1),
    
    % Test invalid state
    Result2 = messaging:handle_message(survey_app, invalid_state, [], "1"),
    ?assertMatch({error, invalid_state}, Result2).

test_agri_survey_app() ->
    % Test farm type selection
    {ok, "farm_size", _, _} = messaging:handle_message(agri_survey_app, "intro", [], "A"),
    
    % Test farm size for crop farming
    History1 = [{"intro", "A"}],
    {ok, NextId1, _, _} = messaging:handle_message(agri_survey_app, "farm_size", History1, "50"),
    ?assertEqual("crop_questions", NextId1),
    
    % Test livestock path
    {ok, "farm_size", _, _} = messaging:handle_message(agri_survey_app, "intro", [], "B"),
    
    % Test mixed farming
    {ok, "farm_size", _, _} = messaging:handle_message(agri_survey_app, "intro", [], "C"),
    History2 = [{"intro", "C"}],
    {ok, NextId2, _, _} = messaging:handle_message(agri_survey_app, "farm_size", History2, "100"),
    ?assertNotEqual("farm_size", NextId2).

test_agri_survey_app_errors() ->
    % Test invalid farm type
    Result1 = messaging:handle_message(agri_survey_app, "intro", [], "D"),
    ?assertMatch({error, _}, Result1),
    
    % Test invalid farm size
    History = [{"intro", "A"}],
    Result2 = messaging:handle_message(agri_survey_app, "farm_size", History, "-1"),
    ?assertMatch({error, _}, Result2),
    
    % Test invalid state
    Result3 = messaging:handle_message(agri_survey_app, "invalid_state", [], "A"),
    ?assertMatch({error, _}, Result3).

test_history_management() ->
    % Create sequential history
    {ok, rice1, _, _} = messaging:handle_message(example_app, intro, [], "1"),
    History1 = [{intro, "1"}],
    
    {ok, rice2, _, _} = messaging:handle_message(example_app, rice1, History1, "next"),
    History2 = [{rice1, "next"} | History1],
    
    % Verify history affects navigation
    {ok, rice3, _, _} = messaging:handle_message(example_app, rice2, History2, "next"),
    
    % Test survey with history-based branching
    SurveyHistory = [{intro, "1"}],
    Result = messaging:handle_message(survey_app, rice1, SurveyHistory, "2"),
    ?assertMatch({ok, _, _, _}, Result).

test_initial_app_data() ->
    % Verify example app initial structure
    Apps = messaging:apps(),
    {example_app, ExampleData} = lists:keyfind(example_app, 1, Apps),
    
    % Check intro state
    IntroState = proplists:get_value(intro, ExampleData),
    ?assertMatch(#app_state{}, IntroState),
    ?assert(is_function(IntroState#app_state.logic)),
    ?assert(string:str(IntroState#app_state.msg, "Choose") > 0),
    
    % Check rice states exist
    ?assertMatch(#app_state{}, proplists:get_value(rice1, ExampleData)),
    ?assertMatch(#app_state{}, proplists:get_value(rice2, ExampleData)),
    
    % Verify survey app structure
    {survey_app, SurveyData} = lists:keyfind(survey_app, 1, Apps),
    ?assert(lists:all(fun(Q) -> is_record(Q, question) end, SurveyData)),
    
    % Verify agri survey app structure
    {agri_survey_app, AgriData} = lists:keyfind(agri_survey_app, 1, Apps),
    ?assert(maps:is_key(questions, AgriData)),
    ?assert(maps:is_key(rules, AgriData)).
