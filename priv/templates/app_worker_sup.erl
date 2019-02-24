%%%-------------------------------------------------------------------
%%% @doc
%%%     {{name}} worker supervisor
%%% @end
%%%-------------------------------------------------------------------
-module({{name}}_worker_sup).

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
    RestartStrategy = simple_one_for_one ,
    MaxRestarts = 3 ,
    MaxSecondsBetweenRestarts = 3600 ,

    SupFlags = {RestartStrategy , MaxRestarts , MaxSecondsBetweenRestarts} ,



    Worker = worker_specs() ,

    {ok , {SupFlags , [Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



%% @private
-spec worker_specs() -> supervisor:child_spec().
worker_specs() ->
    Restart = transient ,
    Shutdown = 2000 ,
    Type = worker ,
    #{id => {{name}}_worker
        , start    => {{{name}}_worker , start_link , []}
        , restart  => Restart
        , shutdown => Shutdown
        , type     => Type
        , modules  => [{{name}}_worker]
    }.