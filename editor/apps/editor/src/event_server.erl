
-module(event_server).

-behaviour(gen_server).

%% API
-export([start_link/0, connect_user/0, disconnect_user/0, broadcast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients = [], last_site_id = 0, events = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

connect_user() ->
    gen_server:call(?MODULE, {connect_user, self()}).

disconnect_user() ->
    gen_server:cast(?MODULE, {disconnect_user, self()}).

broadcast(Msg) ->
    gen_server:cast(?MODULE, {broadcast, self(), Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({connect_user, Pid}, _From, S) ->
    NextSiteId = S#state.last_site_id + 1,
    NowClients = [{Pid, NextSiteId}|S#state.clients],
    {reply, { connected, NextSiteId, S#state.events }, S#state{last_site_id = NextSiteId, clients = NowClients} };
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({disconnect_user, Pid}, S) ->
    RemainingClients = lists:filter(fun ({EPid, _ESiteId}) -> EPid /= Pid end, S#state.clients),
    {noreply, S#state{clients = RemainingClients}};
handle_cast({broadcast, Pid, Msg}, S) ->
    NewEvents = [Msg|S#state.events],
    FilteredClients = lists:filter(fun ({EPid, _ESiteId}) -> EPid /= Pid end, S#state.clients),
    lists:foreach(fun ({CPid, _CId}) -> CPid ! { msg_from_other, Msg } end, FilteredClients),
    {noreply, S#state{events = NewEvents}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
