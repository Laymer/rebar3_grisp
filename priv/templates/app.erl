% @doc {{name}} public API.
% @end
-module({{name}}).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    ok.
%%    {{name}}_sup:start_link().

stop(_State) -> ok.
