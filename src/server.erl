%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson
%%% @copyright (C) 2010, Niclas Axelsson
%%% @doc
%%% The main server
%%% @end
%%% Created : 22 Oct 2010 by Niclas Axelsson
%%%-------------------------------------------------------------------
-module(server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("../include/types.hrl").
-include("../include/server_packets.hrl").

-record(state, {
	  socket,
	  port
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    case gen_tcp:listen(Port, [binary, {active, false}, {packet, 0}]) of
	{ok, LSocket} ->
	    {ok, Socket} = gen_tcp:accept(LSocket),

	    {ok, Binary} = gen_tcp:recv(Socket, 0),
	    P = parser:parse_header(Binary),
	    io:format("~p~n", [P]),

	    Packet = generator:generate_header(#handshake{connection_hash = "-"}),
	    gen_tcp:send(Socket, Packet),
	    
	    {ok, Binary2} = gen_tcp:recv(Socket, 0),
	    P2 = parser:parse_header(Binary2),
	    io:format("~p~n", [P2]),

	    Packet2 = generator:generate_header(#login_response{}),
	    gen_tcp:send(Socket, Packet2),

	    {ok, Binary3} = gen_tcp:recv(Socket, 0),
	    P3 = parser:parse_header(Binary3),
	    io:format("~p~n", [P3]),
	    
	    {ok, #state{socket = Socket, port = Port}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
