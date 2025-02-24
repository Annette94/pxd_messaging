%%%-------------------------------------------------------------------
%%% @doc Messaging Application Interface Module
%%%
%%% This module serves as the main interface for handling different messaging
%%% applications (example_app, survey_app, agri_survey_app). It manages app
%%% versioning, message handling, and state transitions for each app type.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(messaging).

-export([
    init/0,
    apps/0, apps/1,
    handle_message/4, get_current_message/2,
    create_or_update_question/4
]).

-record(app_state, {msg, logic}).
-record(question, {id, msg, opts = [], anchor = []}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initializes the messaging system and ensures default versions for all apps.
-spec init() -> ok.
init() ->
    timer:sleep(1000),
    ensure_app_version(example_app),
    ensure_app_version(survey_app),
    ensure_app_version(agri_survey_app),
    ok.

%% @doc Returns all apps with their latest versions.
-spec apps() -> [{atom(), term()}].
apps() ->
    [
        {example_app, get_app_data(example_app)},
        {survey_app, get_app_data(survey_app)},
        {agri_survey_app, get_app_data(agri_survey_app)}
    ].

%% @doc Returns all apps with specific version.
-spec apps(string()) -> [{atom(), term()}].
apps(VersionId) ->
    [
        {example_app, get_app_data(example_app, VersionId)},
        {survey_app, get_app_data(survey_app, VersionId)},
        {agri_survey_app, get_app_data(agri_survey_app, VersionId)}
    ].

%% @doc Handles an incoming message for a specific app and state.
-spec handle_message(atom(), term(), list(), string()) ->
    {ok, term(), string(), string()} | {error, term()} | ok.
handle_message(AppName, CurrentState, History, Message) ->
    App = proplists:get_value(AppName, apps()),
    case AppName of
        example_app ->
            handle_example_app(App, CurrentState, Message);
        survey_app ->
            handle_survey_app(App, CurrentState, Message);
        agri_survey_app ->
            handle_agri_survey(App, CurrentState, History, Message)
    end.

%% @doc Gets the current message for a specific app and state.
-spec get_current_message(atom(), term()) ->
    {ok, displayed} | {error, invalid_state}.
get_current_message(AppName, CurrentState) ->
    App = proplists:get_value(AppName, apps()),
    case AppName of
        example_app ->
            display_example_app_message(App, CurrentState, AppName);
        survey_app ->
            display_survey_app_message(App, CurrentState, AppName);
        agri_survey_app ->
            display_agri_survey_message(App, CurrentState, AppName)
    end.

%messaging:create_or_update_question(example_app, intro,
%    "Choose topic",
%    fun(Resp) -> case Resp of "1" -> rice1; "2" -> cotton1; _ -> intro end end).

% Will update existing if 'intro' exists
%messaging:create_or_update_question(survey_app, intro,
%    "New topic choice",
%    ["1", "2", "3"]).

% Will update or create for agri_survey_app
%messaging:create_or_update_question(agri_survey_app, "farm_type",
%    "What type of farming do you do?",
%    #{
%        type => single_choice,
%        options => [
%            #{id => "A", text => "Crop Farming"},
%            #{id => "B", text => "Livestock Farming"}
%        ]
%    }).
create_or_update_question(AppName, Id, Msg, Extra) ->
    CurrentData = get_app_data(AppName),
    Action = case exists(AppName, Id, CurrentData) of true -> "Update"; false -> "Create" end,
    {ok, NewVersionId} = app_versioning:create_version(AppName, 
        #{name => Action ++ " for " ++ if is_atom(Id) -> atom_to_list(Id); true -> Id end}),
    
    NewState = case AppName of
        example_app when is_function(Extra) -> 
            case proplists:get_value(Id, CurrentData) of
                #app_state{} = OldState -> lists:keystore(Id, 1, CurrentData, {Id, OldState#app_state{msg = Msg, logic = Extra}});
                _ -> lists:keystore(Id, 1, CurrentData, {Id, #app_state{msg = Msg, logic = Extra}})
            end;
        survey_app when is_list(Extra) -> 
            case lists:keyfind(Id, #question.id, CurrentData) of
                #question{} = OldQ -> lists:keystore(Id, #question.id, CurrentData, OldQ#question{msg = Msg, opts = Extra});
                _ -> lists:keystore(Id, #question.id, CurrentData, #question{id = Id, msg = Msg, opts = Extra})
            end;
        agri_survey_app when is_map(Extra) ->
            Questions = maps:get(questions, CurrentData, #{}),
            case maps:get(Id, Questions, undefined) of
                undefined -> CurrentData#{questions => maps:put(Id, Extra#{text => Msg}, Questions)};
                OldQ -> CurrentData#{questions => maps:put(Id, maps:merge(OldQ#{text => Msg}, Extra), Questions)}
            end
    end,
    
    app_versioning:save_app_data(AppName, NewVersionId, NewState),
    {ok, NewVersionId}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Ensures an app has at least one version.
-spec ensure_app_version(atom()) -> ok.
ensure_app_version(AppName) ->
    case app_versioning:get_latest_version(AppName) of
        {error, no_versions} ->
            %% Create initial version
            {ok, VersionId} = app_versioning:create_version(AppName, #{
                name => "Initial Version",
                created_at => calendar:local_time()
            }),
            io:format(" ======> Created initial version ~p for ~p~n", [VersionId, AppName]),
            app_versioning:save_app_data(AppName, VersionId, get_default_app_data(AppName));
        {ok, _} ->
            ok
    end.

%% @doc Gets app data with defaults if not found.
-spec get_app_data(atom()) -> term().
get_app_data(AppName) ->
    {ok, Data} = app_versioning:get_app_data(AppName, get_default_app_data(AppName)),
    Data.

%% @doc Gets app data for specific version with defaults if not found.
-spec get_app_data(atom(), string()) -> term().
get_app_data(AppName, VersionId) ->
    {ok, Data} = app_versioning:get_app_data(AppName, VersionId, get_default_app_data(AppName)),
    Data.

%% @doc Displays message for example app.
-spec display_example_app_message(list(), atom(), atom()) ->
    {ok, displayed} | {error, invalid_state}.
display_example_app_message(App, CurrentState, AppName) ->
    case proplists:get_value(CurrentState, App) of
        #app_state{msg = CurrentMsg} ->
            io:format("~n~p (~p): ~s~n", [AppName, CurrentState, CurrentMsg]),
            {ok, displayed};
        undefined ->
            {error, invalid_state}
    end.

%% @doc Displays message for survey app.
-spec display_survey_app_message(list(), atom(), atom()) ->
    {ok, displayed} | {error, invalid_state}.
display_survey_app_message(App, CurrentState, AppName) ->
    case lists:keyfind(CurrentState, #question.id, App) of
        #question{msg = CurrentMsg} ->
            io:format("~n~p (~p): ~s~n", [AppName, CurrentState, CurrentMsg]),
            {ok, displayed};
        undefined ->
            {error, invalid_state}
    end.

%% @doc Displays message for agri survey app.
-spec display_agri_survey_message(term(), string(), atom()) ->
    {ok, displayed} | {error, invalid_state}.
display_agri_survey_message(SurveyFun, CurrentState, AppName) when is_function(SurveyFun) ->
    Survey = SurveyFun(),
    display_agri_survey_message(Survey, CurrentState, AppName);
display_agri_survey_message(Survey, CurrentState, _AppName) ->
    Questions = maps:get(questions, Survey),
    case maps:find(CurrentState, Questions) of
        {ok, Question} ->
            agri_survey:present_question(Question),
            {ok, displayed};
        error ->
            {error, invalid_state}
    end.

%% @doc Handles messages for example app.
-spec handle_example_app(list(), atom(), string()) ->
    {ok, atom(), string(), string()} | {error, term()}.
handle_example_app(App, CurrentState, Message) ->
    case proplists:get_value(CurrentState, App) of
        #app_state{msg = CurrentMsg, logic = Logic} ->
            NextState = Logic(Message),
            case proplists:get_value(NextState, App) of
                #app_state{msg = NextMsg} ->
                    {ok, NextState, NextMsg, CurrentMsg};
                undefined ->
                    {error, invalid_state}
            end;
        undefined ->
            {error, invalid_state}
    end.

%% @doc Handles messages for survey app.
-spec handle_survey_app(list(), atom(), string()) ->
    {ok, atom(), string(), string()} | {error, term()}.
handle_survey_app(App, CurrentState, Message) ->
    case lists:keyfind(CurrentState, #question.id, App) of
        #question{msg = CurrentMsg, opts = Opts, anchor = Anchor} = Q ->
            case Opts of
                [] ->
                    NextQ = get_next_question(Q, App),
                    {ok, NextQ#question.id, NextQ#question.msg, CurrentMsg};
                _ ->
                    case lists:member(Message, Opts) of
                        true ->
                            AnchorQs = anchor_qs([{Q#question.id, Message} | Anchor], App),
                            case AnchorQs of
                                [NextQ|_] -> 
                                    {ok, NextQ#question.id, NextQ#question.msg, CurrentMsg};
                                [] -> 
                                    case lists:keyfind(intro, #question.id, App) of
                                        #question{msg = IntroMsg} ->
                                            {ok, intro, IntroMsg, CurrentMsg};
                                        _ ->
                                            {error, invalid_state}
                                    end
                            end;
                        false ->
                            {error, invalid_option}
                    end
            end;
        false ->
            {error, invalid_state}
    end.

%% @doc Handles messages for agri survey app.
-spec handle_agri_survey(term(), string(), list(), string()) ->
    {ok, string(), map(), map()} | {error, term()} | ok.
handle_agri_survey(SurveyFun, CurrentState, History, Message) when is_function(SurveyFun) ->
    Survey = SurveyFun(),
    handle_agri_survey(Survey, CurrentState, History, Message);
handle_agri_survey(Survey, CurrentState, History, Message) ->
    Questions = maps:get(questions, Survey),
    Rules = maps:get(rules, Survey),
    Question = maps:get(CurrentState, Questions, invalid_state),

    case Question of
        invalid_state ->
            {error, invalid_state};
        _ ->
            case agri_survey:process_answer(Question, Message) of
                {error, ErrorMsg} ->
                    {error, ErrorMsg};
                "end" ->
                    ok;
                Answer ->
                    case agri_survey:find_next_question(CurrentState, Question, Answer, Rules, History) of
                        {ok, NextQuestionId} ->
                            NextQuestion = maps:get(NextQuestionId, Questions),
                            agri_survey:present_question(NextQuestion),
                            {ok, NextQuestionId, NextQuestion, Question};
                        {error, ErrorMsg} ->
                            {error, ErrorMsg}
                    end
            end
    end.

%% @doc Gets the next question in sequence.
-spec get_next_question(#question{}, list()) -> #question{}.
get_next_question(CurrentQ, App) ->
    case lists:dropwhile(
        fun(#question{id = Qid}) -> Qid =/= CurrentQ#question.id end, 
        App) of
        [_Current, Next | _] -> Next;
        _ -> lists:keyfind(intro, #question.id, App)
    end.

%% @doc Gets questions matching anchor conditions.
-spec anchor_qs(list(), list()) -> [#question{}].
anchor_qs(Anchor, App) ->
    [Q || Q <- App, Q#question.anchor =:= Anchor].

%% @doc Checks if a question ID exists in current app data.
-spec exists(atom(), term(), term()) -> boolean().
exists(AppName, Id, CurrentData) ->
    case AppName of
        example_app -> proplists:is_defined(Id, CurrentData);
        survey_app -> lists:keymember(Id, #question.id, CurrentData);
        agri_survey_app -> maps:is_key(Id, maps:get(questions, CurrentData, #{}))
    end.

%======================================================
%% Default app data (for initialization)
get_default_app_data(example_app) ->
    [{intro, #app_state{msg = "Choose a topic: (1) Rice (2) Cotton",
                        logic = fun (Resp) -> case Resp of
                                              "1" -> rice1;
                                              "2" -> cotton1;
                                              _Otherwise -> intro
                                          end
                                end}},
     {rice1, #app_state{msg = "Rice is the seed of the grass species Oryza glaberrima or Oryza sativa.",
                        logic = fun (_) -> rice2 end}},
     {rice2, #app_state{msg = "As a cereal grain, it is the most widely consumed staple food for a large part of the world's human population, especially in Asia and Africa.",
                        logic = fun (_) -> rice3 end}},
     {rice3, #app_state{msg = "It is the agricultural commodity with the third-highest worldwide production, after sugarcane and maize.",
                        logic = fun (_) -> intro end}},
     {cotton1, #app_state{msg = "Cotton is a soft, fluffy staple fiber that grows in a boll, or protective case, around the seeds of the cotton plants of the genus Gossypium in the mallow family Malvaceae.",
                        logic = fun (_) -> cotton2 end}},
     {cotton2, #app_state{msg = "The fiber is almost pure cellulose. Under natural conditions, the cotton bolls will increase the dispersal of the seeds.",
                        logic = fun (_) -> intro end}}];

get_default_app_data(survey_app) ->
    [#question{id = intro,
               msg = "Choose a topic: (1) Rice (2) Cotton",
               opts = ["1", "2"]},
     #question{id = rice1,
               msg =
                   "Rice is the seed of the grass species Oryza glaberrima or Oryza "
                   "sativa. Choose a topic: (2) Rice2 (3) Rice3",
               opts = ["2", "3"],
               anchor = [{intro, "1"}]},
     #question{id = rice2,
               msg =
                   "As a cereal grain, it is the most widely consumed staple food "
                   "for a large part of the world's human population, especially "
                   "in Asia and Africa.",
               anchor = [{rice1, "2"}, {intro, "1"}]},
     #question{id = rice3,
               msg =
                   "It is the agricultural commodity with the third-highest worldwide "
                   "production, after sugarcane and maize.",
               anchor = [{rice1, "3"}, {intro, "1"}]},
     #question{id = cotton1,
               msg =
                   "Cotton is a soft, fluffy staple fiber that grows in a boll, "
                   "or protective case, around the seeds of the cotton plants of "
                   "the genus Gossypium in the mallow family Malvaceae.",
               anchor = [{intro, "2"}]},
     #question{id = cotton2,
               msg =
                   "The fiber is almost pure cellulose. Under natural conditions, "
                   "the cotton bolls will increase the dispersal of the seeds.",
               anchor = [{intro, "2"}]}];

get_default_app_data(agri_survey_app) ->
    {ok, Survey} = agri_survey:create_survey(),
    Survey.
