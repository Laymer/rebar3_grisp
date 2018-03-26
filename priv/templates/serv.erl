-module({{name}}_serv).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, run/2]).


-define(WORKER_SPEC(MFA),
                    #{id => worker,
                    start => {{{name}}_worker_sup, start_link, [MFA]},
                    restart => temporary,
                    type => supervisor,
                    shutdown => 15000,
                    modules => [{{name}}_worker_sup]}).

-record(state, {sup,
                workers}).

start_link(Name, Sup, MFA) ->
    gen_server:start_link({local, Name}, ?MODULE, {Sup, MFA}, []).

%% Gen server
init({Sup, MFA}) ->
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{workers=gb_sets:empty()}}.

run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

handle_call({run, Args}, _From, S = #state{sup=Sup, workers=R}) ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok,Pid}, S#state{workers=gb_sets:add(Ref,R)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{workers=Refs}) ->
    io:format("received down msg~n"),
    case gb_sets:is_element(Ref, Refs) of
        true ->
            {noreply, S#state{workers=gb_sets:delete(Ref,Refs)}};
        false -> %% Not our responsibility
            {noreply, S}
    end;

handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?WORKER_SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup=Pid}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
