%%% @doc
%%% Types used in the server
%%% @end

-define(mc_byte(X), X:8/big-signed-integer).
-define(mc_short(X), X:16/big-signed-integer).
-define(mc_int(X), X:32/big-signed-integer).
-define(mc_long(X), X:64/big-signed-integer).
-define(mc_float(X), X:32/big-float).
-define(mc_double(X), X:64/big-float).
-define(mc_bool(X), X:8/big-unsigned-integer).
