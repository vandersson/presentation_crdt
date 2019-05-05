%%%-------------------------------------------------------------------
%% @doc editor public API
%% @end
%%%-------------------------------------------------------------------

-module(editor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
	       {"/", cowboy_static, {priv_file, editor, "index.html"}},
	       {"/websocket", ws_handler, []},
	       {"/static/[...]", cowboy_static, {priv_dir, editor, "static"}}
	      ]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    editor_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
