%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2010, Niclas Axelsson
%%% @doc
%%% Parser for messages sent from client-side
%%% @end
%%% Created : 22 Oct 2010 by Niclas Axelsson <niclas@burbas.se>

-module(parser).

-include("../include/client_packets.hrl").
-include("../include/types.hrl").

-export([parse_header/1]).

parse_header(<<?mc_byte(PacketID), Rest/binary>>) ->
    case PacketID of
	1 ->
	    <<?mc_int(ProtocolVersion), ?mc_short(UsernameLength), RRest/binary>> = Rest,
	    ULength = 8*UsernameLength,
	    <<Username:ULength/native-signed-integer, ?mc_short(_PasswordLength), Password/binary>> = RRest,
	    #login_request{
			    protocol_version = ProtocolVersion,
			    username = Username, 
			    password = Password
			  };
	2 ->
	    <<?mc_short(_Length), Username/binary>> = Rest,
	    #handshake{
			username = erlang:binary_to_list(Username)
		      };
	3 ->
	    <<?mc_short(_Length), Message/binary>> = Rest,
	      #chat_message{
			     message = Message
			   };
	5 ->
	    <<?mc_int(Type), ?mc_short(Count), Payload/binary>> = Rest,
	    ParsedPayload = parse_payload(Payload),
	    #player_inventory{
			       type = Type,
			       count = Count,
			       payload = ParsedPayload
			     };
	10 ->
	    <<?mc_byte(OnGround)>> = Rest,
	    OG = case OnGround of
		     0 -> false;
		     1 -> true
		 end,
	    #player{
		     on_ground = OG
		  };
	11 ->
	    <<?mc_double(X), ?mc_double(Y), ?mc_double(Stance), ?mc_double(Z), ?mc_byte(OnGround)>> = Rest,
	    OG = case OnGround of
		     0 -> false;
		     1 -> true
		 end,
	    #player_position{
		   x = X,
		   y = Y,
		   stance = Stance,
		   z = Z,
		   on_ground = OG
		  };
	12 ->
	    <<?mc_float(Yaw), ?mc_float(Pitch), ?mc_byte(OnGround)>> = Rest,
	    OG = case OnGround of
		     0 -> false;
		     1 -> true
		 end,
	    #player_look{
		   yaw = Yaw,
		   pitch = Pitch,
		   on_ground = OG
		  };
	13 ->
	    <<?mc_double(X), ?mc_double(Y), ?mc_double(Stance), ?mc_double(Z), ?mc_float(Yaw), ?mc_float(Pitch), ?mc_byte(OnGround)>> = Rest,
	    OG = case OnGround of
		     0 -> false;
		     1 -> true
		 end,
	    #player_position_and_look{
		   x = X,
		   y = Y,
		   stance = Stance,
		   z = Z,
		   yaw = Yaw,
		   pitch = Pitch,
		   on_ground = OG
		  };
	14 ->
	    <<?mc_byte(Status), ?mc_int(X), ?mc_byte(Y), ?mc_int(Z), ?mc_byte(Face)>> = Rest,
	    #player_digging{
			     status = Status,
			     x = X,
			     y = Y,
			     z = Z,
			     face = Face
			   };
	15 ->
	    <<?mc_short(ItemID), ?mc_int(X), ?mc_byte(Y), ?mc_int(Z), ?mc_byte(Direction)>> = Rest,
	    #player_block_placement{
				     item_id = ItemID,
				     x = X,
				     y = Y,
				     z = Z,
				     direction = Direction
				   };
	16 ->
	    <<?mc_int(_Unused), ?mc_short(ItemID)>> = Rest,
	    #holding_change{
			     item_id = ItemID
			   };
	255 ->
	    <<?mc_short(_Length), Reason/binary>> = Rest,
	    #disconnect{
	      reason = Reason
	     };
	_ ->
	    error
    end.
			 
			 
			 
	    
	    



parse_payload(Payload) ->
    parse_payload(Payload, []).

parse_payload(<<>>, Acc) ->
    lists:reverse(Acc);
parse_payload(<<?mc_short(-1), Payload/binary>>, Acc) ->
    parse_payload(Payload, [{empty} | Acc]);
parse_payload(<<?mc_short(ItemID), ?mc_byte(Count), ?mc_short(Health), Payload/binary>>, Acc) ->
    parse_payload(Payload, [{ItemID, Count, Health} | Acc]).

    
		
