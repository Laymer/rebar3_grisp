% @doc {{name}} public API.
% @end
-module({{name}}).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

-export([stress/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    ok.

stop(_State) -> ok.

stress({M,F,A}) ->
    {{name}}_sup:start_stress({M,F,A}).
