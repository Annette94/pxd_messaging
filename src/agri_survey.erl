%%%-------------------------------------------------------------------
%%% @doc Agriculture Survey Module
%%%
%%% This module implements a rule-based agricultural survey system that adaptively
%%% presents questions based on previous responses. The survey covers farm types,
%%% sizes, farming practices, income satisfaction, and climate impact assessment.
%%%
%%% @author Annette Jebastina
%%% @copyright Annette Jebastina
%%% @version 1.0.0
%%%-------------------------------------------------------------------

-module(agri_survey).
-export([create_survey/0,
         evaluate_condition/2,
         present_question/1,
         process_answer/2,
         find_next_question/5,
         display_error/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Create an agriculture survey
-spec create_survey() -> {ok, #{questions => map(), rules => list()}}.
create_survey() ->
    Questions = define_questions(),
    Rules = define_rules(),
    
    Survey = #{
        questions => Questions, 
        rules => Rules
    },
    {ok, Survey}.

%% @doc Present the question to the user
-spec present_question(map()) -> ok.
present_question(Question) ->
    Type = maps:get(type, Question),
    Options = case Type of
        single_choice -> maps:get(options, Question, []);
        multiple_choice -> maps:get(options, Question, []);
        _ -> []
    end,

    OptionsText = lists:map(fun(#{id := Id, text := Text}) ->
        io_lib:format("~s) ~s", [Id, Text])
    end, Options),

    FormattedOptions = string:join(OptionsText, ", "),

    case Options of
        [] -> io:format("~n~s~n", [maps:get(text, Question)]);
        _ -> io:format("~n~s: ~s~n", [maps:get(text, Question), FormattedOptions])
    end.


%% @doc Process and validate answer based on question type
-spec process_answer(map(), string()) -> term() | {error, string()}.
process_answer(Question, Input) ->
    Type = maps:get(type, Question),
    
    case Type of
        single_choice -> 
            Options = maps:get(options, Question, []),
            ValidOptions = lists:map(fun(#{id := Id}) -> Id end, Options),
            case lists:member(Input, ValidOptions) of
                true -> Input;
                false -> {error, maps:get(error_message, Question)}
            end;
        
        multiple_choice -> 
            InputList = string:split(Input, ",", all),
            Options = maps:get(options, Question, []),
            ValidOptions = lists:map(fun(#{id := Id}) -> Id end, Options),
            % Check if all inputs are valid options
            case lists:all(fun(Choice) -> lists:member(Choice, ValidOptions) end, InputList) of
                true -> InputList;
                false -> {error, maps:get(error_message, Question)}
            end;
        
        scale -> 
            try
                {Value, _} = string:to_integer(Input),
                Min = maps:get(scale_min, Question, 1),
                Max = maps:get(scale_max, Question, 10),
                case Value >= Min andalso Value =< Max of
                    true -> Value;
                    false -> {error, maps:get(error_message, Question)}
                end
            catch
                _:_ -> {error, maps:get(error_message, Question)}
            end;
        
        text -> 
            case string:is_empty(string:trim(Input)) of
                true -> {error, maps:get(error_message, Question)};
                false -> Input
            end;
        
        numeric -> 
            try
                {Value, _} = string:to_integer(Input),
                case Value > 0 of  % Assuming we want positive numbers
                    true -> Value;
                    false -> {error, maps:get(error_message, Question)}
                end
            catch
                _:_ -> {error, maps:get(error_message, Question)}
            end;
        
        _ -> "end"
    end.

%% @doc Find the next question based on rules
-spec find_next_question(string(), map(), term(), list(), list()) -> {ok, string()} | {error, string()}.
find_next_question(CurrentId, Question, Answer, Rules, Responses) ->
    % Check if Answer contains an error
    case Answer of
        {error, ErrorMsg} ->
            {error, ErrorMsg};
        _ ->
            % Filter rules that apply to current question
            ApplicableRules = lists:filter(
                fun(#{from_id := FromId}) -> FromId =:= CurrentId end,
                Rules
            ),
            ErrorMessage = maps:get(error_message, Question, "Your answer doesn't meet the required conditions. Please try again."),
            
            MatchingRules = lists:filter(
                fun(#{condition := Condition}) -> 
                    evaluate_condition(Condition, {CurrentId, Answer, Responses})
                end,
                ApplicableRules
            ),

            case MatchingRules of
                [#{to_id := NextId} | _] -> {ok, NextId};
                [] ->
                    % If no rules match, check for default rule
                    DefaultRules = lists:filter(
                        fun(#{condition := Condition}) -> Condition =:= default end,
                        ApplicableRules
                    ),
                    case DefaultRules of
                        [#{to_id := NextId} | _] -> {ok, NextId};
                        _ -> {error, ErrorMessage}
                    end
            end
    end.

%% @doc Evaluate a condition based on the answer and previous responses
-spec evaluate_condition(term(), {string(), term(), list()}) -> boolean().
evaluate_condition(default, _) -> 
    true;
evaluate_condition({equals, Value}, {_, Answer, _}) when is_list(Value), is_list(Answer) ->
    lists:sort(Answer) =:= lists:sort(Value);
evaluate_condition({equals, Value}, {_, Answer, _}) when is_list(Answer) ->
    lists:member(Value, Answer);
evaluate_condition({equals, Value}, {_, Answer, _}) ->
    Answer =:= Value;
evaluate_condition({gt, Value}, {_, Answer, _}) when is_integer(Answer) ->
    Answer > Value;
evaluate_condition({lt, Value}, {_, Answer, _}) when is_integer(Answer) ->
    Answer < Value;
evaluate_condition(Fun, {CurrQId, Answer, Responses}) when is_function(Fun) ->
    % For custom logic functions
    Fun(CurrQId, Answer, Responses).

-spec display_error(string()) -> ok.
display_error(ErrorMessage) ->
    io:format("~n[ERROR] ~s~n", [ErrorMessage]).

%% @doc Define agriculture survey questions
define_questions() ->
    #{
        %% Question 1: Farm Type (Single Choice)
        "intro" => #{
            id => "intro",
            type => single_choice,
            text => "What type of farming do you primarily engage in?",
            options => [
                #{id => "A", text => "Crop Farming"},
                #{id => "B", text => "Livestock Farming"},
                #{id => "C", text => "Mixed Farming"}
            ],
            check_all_conditions => false,
            error_message => "Please select a valid option (A, B, or C)."
        },

        %% Question 2: Farm Size (Numeric)
        "farm_size" => #{
            id => "farm_size",
            type => numeric,
            text => "What is the size of your farm in acres?",
            check_all_conditions => false,
            error_message => "Please enter a valid positive number."
        },

        %% Question 3A: Crop Questions (only for crop or mixed farmers)
        "crop_questions" => #{
            id => "crop_questions",
            type => multiple_choice,
            text => "Which crops do you grow? (Select all that apply)",
            options => [
                #{id => "A", text => "Grains"},
                #{id => "B", text => "Vegetables"},
                #{id => "C", text => "Fruits"}
            ],
            check_all_conditions => false,
            error_message => "Please select valid crop options (A, B, or C)."
        },

        %% Question 3B: Livestock Questions (only for livestock or mixed farmers)
        "livestock_questions" => #{
            id => "livestock_questions",
            type => multiple_choice,
            text => "Which livestock do you raise? (Select all that apply)",
            options => [
                #{id => "A", text => "Cattle"},
                #{id => "B", text => "Poultry"},
                #{id => "C", text => "Sheep/Goats"}
            ],
            check_all_conditions => false,
            error_message => "Please select valid livestock options (A, B, or C)."
        },

        %% Question 4: Income Satisfaction (Scale)
        "income_satisfaction" => #{
            id => "income_satisfaction",
            type => scale,
            text => "How satisfied are you with your farming income? (1 = Very Dissatisfied, 10 = Very Satisfied)",
            scale_min => 1,
            scale_max => 10,
            check_all_conditions => true,
            error_message => "Please enter a value between 1 and 10."
        },

        %% Question 5A: Income Improvement (only for low satisfaction)
        "income_improvement" => #{
            id => "income_improvement",
            type => text,
            text => "Since you're not very satisfied with your income, what would help improve it?",
            check_all_conditions => false,
            error_message => "Please provide a response about income improvement."
        },

        %% Question 5B: Success Factors (only for high satisfaction)
        "success_factors" => #{
            id => "success_factors",
            type => text,
            text => "Since you're quite satisfied with your income, what factors have contributed to your success?",
            check_all_conditions => false,
            error_message => "Please share the factors that contributed to your success."
        },

        %% Question 6: Climate Impact (Scale)
        "climate_impact" => #{
            id => "climate_impact",
            type => scale,
            text => "How severely has climate change affected your farm? (1 = No Impact, 10 = Severe Impact)",
            scale_min => 1,
            scale_max => 10,
            check_all_conditions => true,
            error_message => "Please enter a value between 1 and 10."
        },

        %% Question 7A: Climate Adaptation (only for high impact)
        "climate_adaptation" => #{
            id => "climate_adaptation",
            type => multiple_choice,
            text => "Which climate adaptation measures have you implemented? (Select all that apply)",
            options => [
                #{id => "A", text => "Drought-resistant varieties"},
                #{id => "B", text => "Water conservation"},
                #{id => "C", text => "Changed planting times"},
                #{id => "D", text => "None yet"}
            ],
            check_all_conditions => false,
            error_message => "Please select valid adaptation options (A through D)."
        },

        %% Question 7B: Future Climate Concerns (only for low impact)
        "future_climate_concerns" => #{
            id => "future_climate_concerns",
            type => single_choice,
            text => "Although climate impacts are low for you now, which future climate concern worries you most?",
            options => [
                #{id => "A", text => "Drought/water scarcity"},
                #{id => "B", text => "Extreme weather events"},
                #{id => "C", text => "Pests and diseases"},
                #{id => "D", text => "Not concerned"}
            ],
            check_all_conditions => false,
            error_message => "Please select a valid option (A through D)."
        },

        %% Thank You Message
        "thank_you" => #{
            id => "thank_you",
            type => information,
            text => "Thank you for completing the survey! Your inputs are valuable."
        }
    }.

%% @doc Define rules with clear condition-based navigation
define_rules() ->
    [
        %% RULE 1: Initial question to farm size for all farmers
        #{from_id => "intro", condition => default, to_id => "farm_size"},
        
        %% RULE 2: Farm size branching based on farm type
        %% Rule 2A: Crop farmers go to crop questions
        #{from_id => "farm_size", 
          condition => fun(_CurrQId, _Answer, Responses) ->
              proplists:get_value("intro", Responses) =:= "A"  % Crop farmers
          end,
          to_id => "crop_questions"},
          
        %% Rule 2B: Livestock farmers go to livestock questions
        #{from_id => "farm_size", 
          condition => fun(_CurrQId, _Answer, Responses) ->
              proplists:get_value("intro", Responses) =:= "B"  % Livestock farmers
          end,
          to_id => "livestock_questions"},
          
        %% Rule 2C: Mixed farmers with small farms (<50 acres) go to livestock first
        #{from_id => "farm_size", 
          condition => fun(_CurrQId, Answer, Responses) ->
              (proplists:get_value("intro", Responses) =:= "C") andalso (Answer < 50)
          end,
          to_id => "livestock_questions"},
          
        %% Rule 2D: Mixed farmers with large farms (>=50 acres) go to crops first
        #{from_id => "farm_size", 
          condition => fun(_CurrQId, Answer, Responses) ->
              (proplists:get_value("intro", Responses) =:= "C") andalso (Answer >= 50)
          end,
          to_id => "crop_questions"},
        
        %% RULE 3: After crop questions
        %% Rule 3A: Mixed farmers who answered crop questions go to livestock questions
        #{from_id => "crop_questions", 
          condition => fun(_CurrQId, _Answer, Responses) ->
              proplists:get_value("intro", Responses) =:= "C"  % Mixed farmers
          end,
          to_id => "livestock_questions"},
          
        %% Rule 3B: Pure crop farmers go directly to income satisfaction
        #{from_id => "crop_questions", condition => default, to_id => "income_satisfaction"},
        
        %% RULE 4: After livestock questions, all go to income satisfaction
        #{from_id => "livestock_questions", condition => default, to_id => "income_satisfaction"},
        
        %% RULE 5: Income satisfaction branching
        %% Rule 5A: Low satisfaction (< 5) goes to improvement question
        #{from_id => "income_satisfaction", condition => {lt, 5}, to_id => "income_improvement"},
        
        %% Rule 5B: High satisfaction (> 7) goes to success factors question
        #{from_id => "income_satisfaction", condition => {gt, 7}, to_id => "success_factors"},
        
        %% Rule 5C: Medium satisfaction (5-7) goes directly to climate impact
        #{from_id => "income_satisfaction", condition => default, to_id => "climate_impact"},
        
        %% RULE 6: After income improvement or success factors, go to climate impact
        #{from_id => "income_improvement", condition => default, to_id => "climate_impact"},
        #{from_id => "success_factors", condition => default, to_id => "climate_impact"},
        
        %% RULE 7: Climate impact branching
        %% Rule 7A: High climate impact (> 6) goes to adaptation question
        #{from_id => "climate_impact", condition => {gt, 6}, to_id => "climate_adaptation"},
        
        %% Rule 7B: Low climate impact (< 4) goes to future concerns question
        #{from_id => "climate_impact", condition => {lt, 4}, to_id => "future_climate_concerns"},
        
        %% Rule 7C: Medium climate impact (4-6) goes directly to thank you
        #{from_id => "climate_impact", condition => default, to_id => "thank_you"},
        
        %% RULE 8: Final paths to thank you message
        #{from_id => "climate_adaptation", condition => default, to_id => "thank_you"},
        #{from_id => "future_climate_concerns", condition => default, to_id => "thank_you"},
        
        %% RULE 9: End survey
        #{from_id => "thank_you", condition => default, to_id => "end"}
    ].
