% @doc {{name}} public API.
% @end
-module({{name}}).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

-export([gen_compute/1]).
%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  {ok, Supervisor} = master_sup:start_link(),
  % {ok, VortexSupervisor} = master_sup:start_{{name}}({{name}}_pinger, {{{name}}_pinger, start_link, ['{{name}}@my_grisp_board']}),
  % {ok, PartisanSupervisor} = master_sup:start_partisan(),
  % {ok, _} = master_sup:start_{{name}}({{name}}_pinger, {{{name}}_pinger, start_link, ['{{name}}@my_grisp_board']}),
  {ok, _} = master_sup:start_{{name}}(pinger, {{{name}}_pinger, start_link, []}),
  {ok, _} = master_sup:start_partisan(),
  {ok, _} = master_sup:start_lasp(),
  {ok, _} = {{name}}_serv:run(pinger, ['station@Laymer']),
  timer:sleep(5000),

  lasp_peer_service:join('station@Laymer'),
  timer:sleep(5000),

  {ok, {_, _, _, _}} = lasp:declare({<<"results">>, state_orset}, state_orset),
  timer:sleep(5000),

  master_sup:stop_{{name}}(pinger),
  timer:sleep(5000),
  {ok, _} = master_sup:start_{{name}}(computation, {{{name}}_generic_computation, start_link, []}),
  timer:sleep(5000),
  {ok, _} = {{name}}_serv:run(computation, [{<<"functions">>, state_orset}]),
  timer:sleep(5000),
  LEDs = [1, 2],
  [grisp_led:flash(L, aqua, 500) || L <- LEDs],
  timer:sleep(5000),
  grisp_led:off(2),
  Random = fun() ->
      {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
  end,
  grisp_led:pattern(1, [{100, Random}]),
  {ok, Supervisor}.

stop(_State) -> ok.
% partisan_peer_service_manager:myself().
% lasp_peer_service:join('station@Laymer').
gen_compute(Fun) ->
  Fun().
