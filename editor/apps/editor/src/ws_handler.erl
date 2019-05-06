-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    { connected, Id, Events } = event_server:connect_user(),
    {reply, {text, jsone:encode(#{<<"type">> => <<"connected">>, 
				  <<"connected">> => #{<<"siteId">> => Id, 
						       <<"events">> => Events}})}, State}.

websocket_handle({text, Msg}, State) ->
    event_server:broadcast(Msg),
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({msg_from_other, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    event_server:disconnect_user(),
    {ok, State}.
