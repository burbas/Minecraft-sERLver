%%%-------------------------------------------------------------------
%%% @author Vadim Bardakov
%%% @copyright (C) 2011, Vadim Bardakov
%%% @doc
%%% tcp backend
%%% @end
%%% Created : 25 Feb 2011 by Vadim Bardakov
%%%-------------------------------------------------------------------
-module(server).
-compile(export_all). %% while testing


%%Starting the server
%%
%% start() start server on default port (25565)
%%
start()->start(25565).
start(Port)-> 
	{ok, LSock} = gen_tcp:listen(25565, [binary,{active, true}]),
	gen_tcp:controlling_process(LSock, spawn(?MODULE,accept_loop,[LSock])),
	put(sock,LSock).

accept_loop(LSock)->
	case gen_tcp:accept(LSock) of
		{ok,ASock} ->
            gen_tcp:controlling_process(ASock, spawn(?MODULE,client_loop,[ASock])),
            accept_loop(LSock);
        Other ->
            io:format("Accept returned ~w~n",[Other])
	end.

client_loop(Sock)->
	receive
        {tcp,Sock,<<ID:8/binary>>} -> put(id,ID),
			client_loop(Sock);
		{tcp,Sock,Data} ->
			ID=get(id),
			parser:parse_header(<< ID,Data/binary>>);
		Other-> io:format("~w resived ~w~n",[self(),Other]),
			ok
    end.

%%
%% Stopping the server
%%
stop()->gen_tcp:close(get(sock)).