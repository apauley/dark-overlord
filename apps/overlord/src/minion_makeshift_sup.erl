%%% @author Andreas Pauley <>
%%% Created :  8 Feb 2013 by Andreas Pauley <>

-module(minion_makeshift_sup).

-export([start_link/2,
         init/1]).

-export([ping_server/1]).

-record(state, {minion,
                ping_server,
                sponge_pid,
                sponge_node}).

start_link(HypnoSpongePid, RemoteNode) ->
  Pid = proc_lib:spawn_link(RemoteNode, ?MODULE, init, [{HypnoSpongePid, node()}]),
  {ok, Pid}.

init({HypnoSpongePid, HypnoSpongeNode}) ->
  process_flag(trap_exit, true),
  log("minion sup for ~p on ~s~n", [HypnoSpongePid, atom_to_list(HypnoSpongeNode)]),

  %% By Overlord decree, in the unlikely event that a hypnosponge dies, all minions must commit suicide out of respect.
  _Ref = erlang:monitor(process, HypnoSpongePid),

  State1 = #state{sponge_pid=HypnoSpongePid,
                  sponge_node=HypnoSpongeNode},
  State2 = start_ping_server(State1),

  State3 = start_minion(State2),
  supervisor_wait(State3).

supervisor_wait(State) ->
  receive
    Message ->
      handle_message(Message, State)
  after 5 ->
      supervisor_wait(State)
  end.

handle_message(_Message={'DOWN', _Ref, process, Sponge, Reason}, #state{sponge_pid=Sponge}) ->
  log("My sponge (~p) died of reason '~p' ;-(. I too see no reason to live...~n",
      [Sponge, Reason]),

  %% I'm linked to my minion, we both die
  erlang:exit(shutdown);
handle_message(Message, State) ->
  log("handle_message unknown message received: ~p~n", [Message]),
  supervisor_wait(State).

start_minion(State = #state{sponge_pid=HypnoSpongePid}) ->
  {ok, Minion} = minion:start_link(HypnoSpongePid),
  State#state{minion=Minion}.

start_ping_server(State = #state{sponge_node=HypnoSpongeNode}) ->
  PingPid = proc_lib:spawn_link(?MODULE, ping_server, [HypnoSpongeNode]),
  State#state{ping_server=PingPid}.

ping_server(Node) ->
  log("ping_server started in order to ping ~p~n", [Node]),
  ping_loop(Node).

ping_loop(Node) ->
  _PingPang = net_adm:ping(Node),
  timer:sleep(1000),
  ping_loop(Node).

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).