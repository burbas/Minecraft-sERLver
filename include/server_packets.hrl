%%% @doc
%%% Records for messages (Server to Client packets)
%%% @end


-record(login_response, {
	  packet_id = 1,
	  a,
	  b,
	  c
	 }).

-record(handshake, {
	  packet_id = 2,
	  connection_hash
	 }).

-record(chat_message, {
	  packet_id = 3,
	  message
	 }).

-record(time_update, {
	  packet_id = 4,
	  time
	 }).

-record(player_inventory, {
	  packet_id = 5,
	  type,
	  count,
	  payload
	 }).

-record(spawn_position, {
	  packet_id = 6,
	  x,
	  y,
	  z
	 }).

-record(player_position_and_look, {
	  packet_id = 13,
	  x,
	  stance,
	  y,
	  z,
	  yaw,
	  pitch,
	  on_ground
	 }).

-record(add_to_inventory, {
	  packet_id = 17,
	  item_type,
	  count,
	  life
	 }).

-record(arm_animation, {
	  packet_id = 18,
	  eid,
	  animate
	 }).

-record(named_entity_spawn, {
	  packet_id = 20,
	  eid,
	  player_name,
	  x,
	  y,
	  z,
	  rotation,
	  pitch,
	  current_item
	 }).

-record(pickup_spawn, {
	  packet_id = 21,
	  eid,
	  item,
	  count,
	  x,
	  y,
	  z,
	  rotation,
	  pitch,
	  roll
	 }).

-record(collect_item, {
	  packet_id = 22, 
	  collected_eid,
	  collector_eid
	 }).

-record(add_object_vehicle, {
	  packet_id = 23,
	  eid,
	  type,
	  x,
	  y,
	  z
	 }).

-record(mob_spawn, {
	  packet_id = 24,
	  eid,
	  type,
	  x,
	  y,
	  z,
	  yaw,
	  pitch
	 }).

-record(destroy_entity, {
	  packet_id = 29,
	  eid
	 }).

-record(entity, {
	  packet_id = 30,
	  eid
	 }).

-record(entity_relative_move, {
	  packet_id = 31,
	  eid,
	  x,
	  y,
	  z
	 }).

-record(entity_look, {
	  packet_id = 32, 
	  eid,
	  yaw,
	  pitch
	 }).

-record(entity_look_and_relative_move, {
	  packet_id = 33,
	  eid,
	  x,
	  y,
	  z,
	  yaw,
	  pitch
	 }).

-record(entity_teleport, {
	  packet_id = 34,
	  eid,
	  x,
	  y,
	  z,
	  yaw,
	  pitch
	 }).

-record(pre_chunk, {
	  packet_id = 50,
	  x,
	  z,
	  mode
	 }).

-record(map_chunk, {
	  packet_id = 51,
	  x,
	  y,
	  z,
	  size_x,
	  size_y,
	  size_z,
	  compressed_chunk_size,
	  compressed_chunk
	 }).

-record(multi_block_change, {
	  packet_id = 52,
	  chunk_x,
	  chunk_z,
	  array_size,
	  coordinate_array,
	  type_array,
	  metadata_array
	 }).

-record(block_change, {
	  packet_id = 53,
	  x,
	  y,
	  z,
	  block_type,
	  block_metadata
	 }).

-record(complex_entities, {
	  packet_id = 59,
	  x,
	  y,
	  z,
	  payload_size,
	  payload
	 }).

-record(kick, {
	  packet_id = 255,
	  reason
	 }).
