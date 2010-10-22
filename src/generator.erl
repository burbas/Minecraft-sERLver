%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2010, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 22 Oct 2010 by Niclas Axelsson <niclas@burbas.se>

-module(generator).

-include("../include/server_packets.hrl").
-include("../include/types.hrl").

-export([generate_header/1]).

generate_header(#login_response{
		   packet_id = ID, 
		   a = _A,
		   b = _B,
		   c = _C
		  }) ->
    <<?mc_byte(ID), ?mc_short(0), ?mc_short(0), ?mc_short(0)>>;

generate_header(#handshake{
		  packet_id = ID,
		  connection_hash = Hash
		  }) ->
    HashBin = erlang:list_to_binary(Hash),
    Size = erlang:size(HashBin),
    <<?mc_byte(ID), ?mc_short(Size), HashBin/binary>>;

generate_header(#chat_message{
		  packet_id = ID,
		  message = Message
		  }) ->
    MessageBin = <<Message/binary>>,
    Size = erlang:size(MessageBin),
    <<?mc_byte(ID), ?mc_short(Size), MessageBin>>;

generate_header(#time_update{
		  packet_id = ID,
		  time = Time
		  }) ->
    <<?mc_byte(ID), ?mc_long(Time)>>;

generate_header(#player_inventory{
		  packet_id = ID,
		  type = Type,
		  count = Count,
		  payload = Payload
		  }) ->
    PayloadBin = generate_payload(Payload),
    <<?mc_byte(ID), ?mc_int(Type), ?mc_short(Count), PayloadBin/binary>>;

generate_header(#spawn_position{
		  packet_id = ID,
		  x = X,
		  y = Y,
		  z = Z
		  }) ->
    <<?mc_byte(ID), ?mc_int(X), ?mc_int(Y), ?mc_int(Z)>>;

generate_header(#player_position_and_look{
		  packet_id = ID,
		  x = X,
		  stance = Stance,
		  y = Y,
		  z = Z,
		  yaw = Yaw,
		  pitch = Pitch,
		  on_ground = OnGround
		  }) ->
    OG = case OnGround of
	     true -> 
		 1;
	     false -> 
		 0
	 end,
    <<?mc_byte(ID), ?mc_double(X), ?mc_double(Stance), ?mc_double(Y), ?mc_double(Z), ?mc_float(Yaw), ?mc_float(Pitch), ?mc_byte(OG)>>;

generate_header(#add_to_inventory{
		  packet_id = ID,
		  item_type = ItemType,
		  count = Count,
		  life = Life
		  }) ->
    <<?mc_byte(ID), ?mc_short(ItemType), ?mc_byte(Count), ?mc_short(Life)>>;

generate_header(#arm_animation{
		  packet_id = ID,
		  eid = EID,
		  animate = Animate
		  }) ->
    Am = case Animate of
	     true -> 1;
	     false -> 0
	 end,
    <<?mc_byte(ID), ?mc_int(EID), ?mc_byte(Am)>>;

generate_header(#named_entity_spawn{
		  packet_id = ID,
		  eid = EID,
		  player_name = PlayerName,
		  x = X,
		  y = Y,
		  z = Z,
		  rotation = Rotation,
		  pitch = Pitch,
		  current_item = CurrentItem
		  }) ->
    PlayerNameBin = erlang:list_to_binary(PlayerName),
    PlayerNameSize = erlang:size(PlayerNameBin),
    <<?mc_byte(ID), ?mc_int(EID), ?mc_short(PlayerNameSize), PlayerNameBin/binary, ?mc_int(X), ?mc_int(Y), ?mc_int(Z), ?mc_byte(Rotation), ?mc_byte(Pitch), ?mc_short(CurrentItem)>>;

generate_header(#pickup_spawn{
		  packet_id = ID,
		  eid = EID,
		  item = Item,
		  count = Count,
		  x = X,
		  y = Y,
		  z = Z,
		  rotation = Rotation,
		  pitch = Pitch,
		  roll = Roll
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_short(Item), ?mc_byte(Count), ?mc_int(X), ?mc_int(Y), ?mc_int(Z), ?mc_byte(Rotation), ?mc_byte(Pitch), ?mc_byte(Roll)>>;

generate_header(#collect_item{
		  packet_id = ID,
		  collected_eid = CollectedEID,
		  collector_eid = CollectorEID
		  }) ->
    <<?mc_byte(ID), ?mc_int(CollectedEID), ?mc_int(CollectorEID)>>;

generate_header(#add_object_vehicle{
		  packet_id = ID,
		  eid = EID,
		  type = Type,
		  x = X,
		  y = Y,
		  z = Z
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_byte(Type), ?mc_int(X), ?mc_int(Y), ?mc_int(Z)>>;

generate_header(#mob_spawn{
		  packet_id = ID,
		  eid = EID,
		  type = Type,
		  x = X,
		  y = Y,
		  z = Z,
		  yaw = Yaw,
		  pitch = Pitch
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_byte(Type), ?mc_int(X), ?mc_int(Y), ?mc_int(Z), ?mc_byte(Yaw), ?mc_byte(Pitch)>>;

generate_header(#destroy_entity{
		   packet_id = ID,
		   eid = EID
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID)>>;

generate_header(#entity{
		  packet_id = ID,
		  eid = EID
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID)>>;

generate_header(#entity_relative_move{
		  packet_id = ID,
		  eid = EID,
		  x = X,
		  y = Y,
		  z = Z
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_byte(X), ?mc_byte(Y), ?mc_byte(Z)>>;

generate_header(#entity_look{
		  packet_id = ID,
		  eid = EID,
		  yaw = Yaw,
		  pitch = Pitch
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_byte(Yaw), ?mc_byte(Pitch)>>;

generate_header(#entity_look_and_relative_move{
		  packet_id = ID,
		  eid = EID,
		  x = X,
		  y = Y,
		  z = Z,
		  yaw = Yaw,
		  pitch = Pitch
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_byte(X), ?mc_byte(Y), ?mc_byte(Z), ?mc_byte(Yaw), ?mc_byte(Pitch)>>;

generate_header(#entity_teleport{
		  packet_id = ID,
		  eid = EID,
		  x = X,
		  y = Y,
		  z = Z,
		  yaw = Yaw,
		  pitch = Pitch
		  }) ->
    <<?mc_byte(ID), ?mc_int(EID), ?mc_int(X), ?mc_int(Y), ?mc_int(Z), ?mc_byte(Yaw), ?mc_byte(Pitch)>>;

generate_header(#pre_chunk{
		  packet_id = ID,
		  x = X,
		  z = Z,
		  mode = Mode
		  }) ->
    Md = case Mode of
	     true -> 0;
	     false -> 1
	 end,
    <<?mc_byte(ID), ?mc_int(X), ?mc_int(Z), ?mc_byte(Md)>>;

generate_header(#map_chunk{
		  packet_id = _ID,
		  x = _X,
		  y = _Y,
		  z = _Z,
		  size_x = _SX,
		  size_y = _SY,
		  size_z = _SZ,
		  compressed_chunk_size = _CCS,
		  compressed_chunk = _CS
		  }) ->
    todo.

%% TODO: Last 4 packets














		 








generate_payload(Payload) ->
    generate_payload(Payload, <<>>).

generate_payload([], Acc) ->
    Acc;
generate_payload([{empty}|Rest], Acc) ->
    generate_payload(Rest, <<?mc_short(-1), Acc/binary>>);
generate_payload([{ItemID, Count, Health}|Rest], Acc) ->
    generate_payload(Rest, <<?mc_short(ItemID), ?mc_byte(Count), ?mc_short(Health), Acc/binary>>).

    
    
