%%% @doc
%%% Records for messages (Client to Server packets)
%%% @end


-record(keep_alive, {
	  packet_id = 0
	 }).

-record(login_request, {
	  packet_id = 1,
	  protocol_version = 2,
	  username,
	  password
	 }).

-record(handshake, {
	  packet_id = 2,
	  username
	 }).

-record(chat_message, {
	  packet_id = 3,
	  message
	 }).

-record(player_inventory, {
	  packet_id = 5,
	  type      = -1,
	  count,
	  payload
	 }).

-record(player, {
	  packet_id = 10,
	  on_ground
	 }).

-record(player_position, {
	  packet_id = 11,
	  x,
	  y,
	  stance,
	  z,
	  on_ground
	 }).

-record(player_look, {
	  packet_id = 12,
	  yaw,
	  pitch,
	  on_ground
	 }).

-record(player_position_and_look, {
	  packet_id = 13,
	  x,
	  y,
	  stance,
	  z,
	  yaw,
	  pitch,
	  on_ground
	 }).

-record(player_digging, {
	  packet_id = 14,
	  status,
	  x,
	  y,
	  z,
	  face
	 }).

-record(player_block_placement, {
	  packet_id = 15,
	  item_id,
	  x,
	  y,
	  z,
	  direction
	 }).

-record(holding_change, {
	  packet_id = 16,
	  unused,
	  item_id
	 }).

-record(disconnect, {
	  packet_id = 255,
	  reason
	 }).

	  
