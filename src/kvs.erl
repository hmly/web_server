-module('kvs').

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() -> register(kvserver, spawn (fun loop/0)).

loop()->
receive
	{From, {store, Key, Value}} -> put(Key, {ok,Value}),From ! {kvserver, true}, loop();
	{From, {lookup, Key}} -> From ! {kvserver, get(Key)}, loop()
end.
