% @doc
% @end
-module({{name}}_service_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->

    {ok, { {one_for_all, 0, 1}, []} }.


%% @private
-spec server_specs() -> supervisor:child_spec().
server_specs() ->
    Restart = permanent ,
    Shutdown = 5000 ,
    Type = worker ,
    #{id => app_server
        , start    => {app_server , start_link , []}
        , restart  => Restart
        , shutdown => Shutdown
        , type     => Type
        , modules  => [app_server]
    }.