%%%-------------------------------------------------------------------
%%% @doc
%%%     {{name}} top level supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module({{name}}_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER , ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok , Pid :: pid()} | ignore | {error , Reason :: term()}).
start_link() ->
    supervisor:start_link({local , ?SERVER} , ?MODULE , []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok , {SupFlags :: {RestartStrategy :: supervisor:strategy() ,
                        MaxR :: non_neg_integer() , MaxT :: non_neg_integer()} ,
           [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error , Reason :: term()}).
init([]) ->
    RestartStrategy = one_for_one ,
    MaxRestarts = 64 ,
    MaxSecondsBetweenRestarts = 13600 ,

    SupFlags = {RestartStrategy , MaxRestarts , MaxSecondsBetweenRestarts} ,
    SupervisorSpecs = sup_specs(),
    ServerSpecs = server_specs(),

    {ok , {SupFlags , [SupervisorSpecs, ServerSpecs]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec server_specs() -> supervisor:child_spec().
server_specs() ->
    Restart = permanent ,
    Shutdown = 5000 ,
    Type = worker ,
    #{id => server
        , start    => {server , start_link , []}
        , restart  => Restart
        , shutdown => Shutdown
        , type     => Type
        , modules  => [server]
    }.


%% @private
-spec sup_specs() -> supervisor:child_spec().
sup_specs() ->
    Restart = permanent ,
    Shutdown = 7200 ,
    Type = supervisor ,
    #{id => app_worker_sup
        , start    => {app_worker_sup , start_link , []}
        , restart  => Restart
        , shutdown => Shutdown
        , type     => Type
        , modules  => [app_worker_sup]
    }.