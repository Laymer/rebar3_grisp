% @doc board top level supervisor.
% @end
-module(master_sup).

-behavior(supervisor).

% API
-export([start_link/0, start_partisan/0, start_{{name}}/2, stop/0, stop_{{name}}/1, start_lasp/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, {{name}}}, ?MODULE, []).

stop() ->
    case whereis({{name}}) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_{{name}}(Name, MFA) ->
    ChildSpec = #{id => Name,
                        start => {{{name}}_sup, start_link, [Name, MFA]},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [{{name}}_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, VortexSup} = supervisor:start_child({{name}}, ChildSpec),
    {ok, VortexSup}.

start_partisan() ->
    partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager),
    ChildSpec = #{id => partisan_sup,
                        start => {partisan_sup, start_link, []},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [partisan_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, PartisanSup} = supervisor:start_child({{name}}, ChildSpec),
    {ok, PartisanSup}.

start_lasp() ->
    ChildSpec = #{id => lasp_sup,
                        start => {lasp_sup, start_link, []},
                        restart => permanent,
                        type => supervisor,
                        shutdown => 15000,
                        modules => [lasp_sup]},
    ok = supervisor:check_childspecs([ChildSpec]),
    {ok, PartisanSup} = supervisor:start_child({{name}}, ChildSpec),
    {ok, PartisanSup}.

stop_{{name}}(Name) ->
    supervisor:terminate_child({{name}}, Name),
    supervisor:delete_child({{name}}, Name).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, { {one_for_all, 1, 5}, []} }.
