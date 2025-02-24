# PXD - Coding Challenge

## Design Approach

When approaching this challenge, I focused on several key design principles:

### 1. OTP Compliance

I structured the application following OTP design principles to ensure reliability and fault tolerance. This meant organizing the code into proper gen_server behaviors and establishing a supervision tree that can recover from failures.

### 2. Clear Separation of Concerns

I separated the codebase into distinct modules with specific responsibilities:
- User management (`messaging_user.erl`)
- Session handling (`messaging_session.erl`)
- Database operations (`messaging_db.erl`)
- Application logic (`messaging.erl`)
- Survey implementations (`agri_survey.erl`)

This separation makes the code more maintainable and easier to extend.

### 3. Stateful Session Design

Rather than implementing stateless interactions, I designed a system where each user has a dedicated session process that maintains conversation state. This allows for:
- User-specific state persistence
- Proper session timeout handling
- Clear ownership of user data

### 4. Data Persistence Strategy

Mnesia for data storage.

### 5. Versioning Approach

For app data (questions) versioning, I implemented a system that:
- Stores metadata with each version
- Maintains historical data across changes
- Provides access to specific versions

## Implementation Details

### Core Requirements

I've maintained the base logic of example_app and survey_app as provided, but removed the receive code block and accommodated it into the structure I've designed. This approach integrates the original functionality into a more robust OTP-based architecture while preserving the original behavior.

### Added a new survey (application) called `agri_survey`

The `agri_survey` module demonstrates a more sophisticated survey system with:

- Dynamic question routing based on previous answers
- Multiple question types (single choice, multiple choice, scale, text, numeric)
- Conditional logic with rule-based progression
- Farm type-specific questions (crop, livestock, mixed farming paths)
- Error handling when wrong input is provided

The key innovation in `agri_survey` is its declarative approach to defining both questions and routing rules separately, making it easy to modify the survey flow without changing the core logic.

### OTP Supervision Structure and Message Flow

I've implemented a proper OTP supervision tree for fault tolerance:

```
messaging_sup (one_for_one)
├── messaging_session_sup (simple_one_for_one)
│   └── messaging_session (gen_server instances for each user session)
└── messaging_session_registry (gen_server for tracking sessions)
```

Here's a diagram showing both the supervision hierarchy and the message flow:

```
                            ┌─────────────────┐
                            │  messaging_app  │
                            └────────┬────────┘
                                     │ starts
                                     ▼
                  ┌─────────────────────────────────────┐
                  │         messaging_sup               │
                  │       (top-level supervisor)        │
                  └──────────┬─────────────────┬────────┘
                             │                 │
                 supervises  │                 │  supervises
                             ▼                 ▼
┌───────────────────────────────────┐ ┌──────────────────────────────┐
│       messaging_session_sup       │ │   messaging_session_registry │
│     (session supervisor)          │ │   (tracks active sessions)    │
└────────────────┬─────────────────┘ └───────────────┬───────────────┘
                 │ creates                            │
                 │                        registers/  │
                 ▼                        looks up    │
┌────────────────────────────────┐                   │
│      messaging_session         │◄──────────────────┘
│      (per-user session)        │
└─────────────────┬──────────────┘
                  │
                  │ uses
                  ▼
┌─────────────────────────────────────────────────────┐
│                   messaging_db                       │
│            (state & history persistence)            │
└────────────────────────┬────────────────────────────┘
                         │ uses
                         ▼
┌─────────────────────────────────────────────────────┐
│                     Mnesia                           │
│              (database storage)                      │
└─────────────────────────────────────────────────────┘
```

This structure ensures that:
- If an individual session crashes, only that user is affected
- If the registry crashes, it can recover without losing session data
- The entire application can be restarted in a controlled manner

#### Message Flow:

1. User requests come in through `messaging_user` functions
2. The `messaging_session_registry` locates the appropriate session process
3. The `messaging_session` process handles the message and updates state
4. State changes are persisted to `messaging_db`
5. Mnesia stores the data durably
6. Responses flow back through the same path

### Installation

#### Prerequisites

- Erlang/OTP (version 21 or later)
- rebar3 (build tool)
- git (version control)

#### 1. Install Erlang/OTP

Install Erlang version > 21

**On Ubuntu/Debian:**
```bash
# Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update

# Install Erlang
sudo apt-get install erlang
```

**On macOS (using Homebrew):**
```bash
brew install erlang
```

**On Windows:**
- Download the installer from [Erlang Solutions](https://www.erlang-solutions.com/downloads/)
- Run the installer and follow the prompts

#### 2. Install rebar3

**On Ubuntu/Debian or macOS:**
```bash
# Download rebar3
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
# Move to a directory in your PATH
sudo mv rebar3 /usr/local/bin/
```

**On Windows:**
- Download from [rebar3.org](https://rebar3.org)
- Add the directory to your PATH

#### 3. Install git

**On Ubuntu/Debian:**
```bash
sudo apt-get install git
```

**On macOS:**
```bash
brew install git
```

**On Windows:**
- Download from [git-scm.com](https://git-scm.com/downloads)
- Run the installer

#### 4. Clone and Build the Project

```bash
# Clone the repository
git clone git@github.com:Annette94/pxd_messaging.git
cd pxd_messaging

# Compile the project
rebar3 compile

# Start Erlang shell with the compiled code
rebar3 shell
```

### Project Structure

The project follows a standard rebar3 structure:

```
messaging/
├── src/               # Source code
├── include/           # Header files
├── test/              # EUnit tests
├── config/            # Configuration files
├── rebar.config       # Build configuration
└── README.md          # Documentation
```

### Example Usage for Each App

#### Message Flow Diagram

When a user interacts with the system, here's how a message flows through the application:

```
┌─────────┐    ┌────────────────┐    ┌───────────────────┐    ┌──────────────┐
│  User   │    │ messaging_user │    │ session_registry  │    │  session     │
└────┬────┘    └───────┬────────┘    └─────────┬─────────┘    └───────┬──────┘
     │                 │                       │                      │
     │ interact        │                       │                      │
     │────────────────►│                       │                      │
     │                 │ get_session_pid       │                      │
     │                 │──────────────────────►│                      │
     │                 │                       │                      │
     │                 │◄──────────────────────│                      │
     │                 │ session_pid           │                      │
     │                 │                       │                      │
     │                 │ gen_server:call       │                      │
     │                 │─────────────────────────────────────────────►│
     │                 │                       │                      │
     │                 │                       │                      │ handle_call
     │                 │                       │                      │◄─────────┐
     │                 │                       │                      │          │
     │                 │                       │                      │          │
     │                 │                       │                      │          │
     │                 │◄─────────────────────────────────────────────┘          │
     │                 │ response             │                      │           │
     │◄────────────────┘                      │                      │           │
     │ ok               │                      │                      │           │
     │                 │                       │                      │ save_state│
     │                 │                       │                      │───────────┘
```

#### example_app

```erlang
% Create a user
messaging_user:create_user("user1", "password123").

% Create a session with example_app
{ok, SessionId1} = messaging_user:create_user_session("user1", "password123", example_app, intro).

% Interact with the session
messaging_user:interact(SessionId1, "1").  % Choose Rice
messaging_user:interact(SessionId1, "tell me more").  % Continue to next state
messaging_user:interact(SessionId1, "interesting").  % Continue to next state

% View history
messaging_user:show_history(SessionId1).
{ok,[{rice2,"interesting"},
     {rice1,"tell me more"},
     {intro,"1"}]}
```

#### survey_app

```erlang
% Create a session with survey_app
{ok, SessionId2} = messaging_user:create_user_session("user1", "password123", survey_app, intro).

% Interact with the session
messaging_user:interact(SessionId2, "1").  % Choose Rice
messaging_user:interact(SessionId2, "2").  % Choose Rice2
messaging_user:interact(SessionId2, "continue").  % Continue to next state
```

#### agri_survey_app

```erlang
% Create a session with agri_survey_app
{ok, SessionId3} = messaging_user:create_user_session("user1", "password123", agri_survey_app, "intro").

% Complete a farm survey
messaging_user:interact(SessionId3, "A").  % Choose Crop Farming
messaging_user:interact(SessionId3, "100").  % Farm size in acres
messaging_user:interact(SessionId3, "A,B").  % Grow grains and vegetables
messaging_user:interact(SessionId3, "8").  % Satisfaction level 8/10
messaging_user:interact(SessionId3, "Good irrigation").  % Success factors
messaging_user:interact(SessionId3, "3").  % Low climate impact
messaging_user:interact(SessionId3, "A").  % Future climate concern

% View history
messaging_user:show_history(SessionId3).
```

## Technical Implementation

### Database with Mnesia

I chose Mnesia for database storage because it's native to Erlang and doesn't require separate installation, which makes testing the code flow easier. Mnesia provides:

1. Transaction support for data consistency
2. Table distribution across nodes
3. In-memory tables with disk persistence
4. Schema flexibility

### State Persistence

State persistence is implemented through the `messaging_db` module:

- Application state is saved to Mnesia after each interaction
- When a user creates a new session, the system checks for previous state
- If previous state exists, it's loaded and used as the starting point
- This ensures continuity of experience across sessions

### User Authentication & Security

User authentication has several security features:

- Usernames are unique identifiers in the system
- Passwords are stored as salted hashes (SHA-256) rather than plaintext
- Each salt is randomly generated for additional security
- Sessions have unique IDs generated using `crypto:strong_rand_bytes/1`
- During each interaction, the session is verified

Sessions time out after a configurable period of inactivity. The timeout value is stored in the application environment:

```erlang
% Get the configured timeout or use default
get_session_timeout() ->
    application:get_env(messaging, session_timeout, ?DEFAULT_TIMEOUT).

% Check if session has timed out
check_session_timeout(State) ->
    Now = erlang:system_time(second),
    Timeout = get_session_timeout(),
    case Now - State#state.last_activity > Timeout of
        true -> {error, session_timeout};
        false -> ok
    end.
```

### App Versioning System

The versioning system provides a comprehensive way to manage different versions of application data. This is particularly useful for tracking changes to survey questions over time.

#### Key Versioning API Functions

```erlang
#### Shell Session Example

Here's a complete example showing the versioning system in action from the Erlang shell:

```erlang
%% List all versions for example_app
1> app_versioning:list_app_versions(example_app).
[{app_version,example_app,
              "13c45d375eae5ca1bac915acf584da91ad6c08b5",1740412029,
              #{name => "Initial Version",
                created_at => {{2025,2,24},{21,17,9}}}}]

%% Get app data for a specific version
2> app_versioning:get_app_data(example_app, "13c45d375eae5ca1bac915acf584da91ad6c08b5", "default").
{ok,[{intro,{app_state,"Choose a topic: (1) Rice (2) Cotton",
                       #Fun<messaging.1.110044560>}},
     {rice1,{app_state,"Rice is the seed of the grass species Oryza glaberrima or Oryza sativa.",
                       #Fun<messaging.2.110044560>}},
     {rice2,{app_state,"As a cereal grain, it is the most widely consumed staple food for a large part of the world's human population, especially in Asia and Africa.",
                       #Fun<messaging.3.110044560>}},
     {rice3,{app_state,"It is the agricultural commodity with the third-highest worldwide production, after sugarcane and maize.",
                       #Fun<messaging.4.110044560>}},
     {cotton1,{app_state,"Cotton is a soft, fluffy staple fiber that grows in a boll, or protective case, around the seeds of the cotton plants of the genus Gossypium in the mallow family Malvaceae.",
                         #Fun<messaging.5.110044560>}},
     {cotton2,{app_state,"The fiber is almost pure cellulose. Under natural conditions, the cotton bolls will increase the dispersal of the seeds.",
                         #Fun<messaging.6.110044560>}}]}

%% Create a new version by updating a question
3> {ok, NewVersionId} = messaging:create_or_update_question(example_app, intro, 
     "Choose topic", 
     fun(Resp) -> case Resp of "1" -> rice1; "2" -> cotton1; _ -> intro end end).
{ok,"a3b78b79bdd956e218fd41d582f1bf6c380dde08"}

%% Get app data for the new version
4> app_versioning:get_app_data(example_app, NewVersionId, "default").
{ok,[{intro,{app_state,"Choose topic",
                       #Fun<erl_eval.42.39164016>}},
     {rice1,{app_state,"Rice is the seed of the grass species Oryza glaberrima or Oryza sativa.",
                       #Fun<messaging.2.110044560>}},
     {rice2,{app_state,"As a cereal grain, it is the most widely consumed staple food for a large part of the world's human population, especially in Asia and Africa.",
                       #Fun<messaging.3.110044560>}},
     {rice3,{app_state,"It is the agricultural commodity with the third-highest worldwide production, after sugarcane and maize.",
                       #Fun<messaging.4.110044560>}},
     {cotton1,{app_state,"Cotton is a soft, fluffy staple fiber that grows in a boll, or protective case, around the seeds of the cotton plants of the genus Gossypium in the mallow family Malvaceae.",
                         #Fun<messaging.5.110044560>}},
     {cotton2,{app_state,"The fiber is almost pure cellulose. Under natural conditions, the cotton bolls will increase the dispersal of the seeds.",
                         #Fun<messaging.6.110044560>}}]}

%% List all versions again - now we have two
5> app_versioning:list_app_versions(example_app).
[{app_version,example_app,
              "a3b78b79bdd956e218fd41d582f1bf6c380dde08",1740412078,
              #{name => "Update for intro"}},
 {app_version,example_app,
              "13c45d375eae5ca1bac915acf584da91ad6c08b5",1740412029,
              #{name => "Initial Version",
                created_at => {{2025,2,24},{21,17,9}}}}]
```

This example demonstrates:
1. Viewing existing versions
2. Retrieving data for a specific version
3. Creating a new version by updating a question
4. Verifying the update was applied correctly
5. Confirming that both versions are preserved in the system

Using this versioning approach, you can track all changes made to your application data over time and easily roll back to previous versions if needed.

## Testing

I've written test files in the `tests` folder using EUnit that cover:

- User creation and authentication
- Session management and timeouts
- App versioning functionality
- Core survey interaction logic

Run the tests with:

```bash
rebar3 eunit
```

## CI/CD Integration

I've set up a GitHub Actions workflow that automatically:

1. Builds the application using rebar3
2. Runs the unit tests
3. Creates a production release
4. Packages it as a deployable tarball
5. Uploads the release as an artifact

This ensures code quality with every push and provides ready-to-deploy artifacts.

### Code Documentation

I've ensured that the code is well-documented throughout:

- **Module Docstrings**: Each module begins with a comprehensive `@doc` docstring explaining its purpose and functionality
- **Function Specifications**: Every exported function includes proper `-spec` type specifications
- **Function Documentation**: Individual functions have `@doc` annotations describing their purpose, parameters, and return values
- **Type Definitions**: Custom types are defined and documented for better code understanding

##

This implementation fulfills all the core requirements and additional tasks while following Erlang/OTP best practices. The modular design makes it easy to extend the system with new features or survey types in the future.
