-module('ss').
-define (SESSION_TIMEOUT, 600000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() -> register(sessionserver, spawn (fun loop/0)).

loop() -> loop([]).

loop(Database) ->
	receive
		{login, Name, Password, sessionserver, Pid} -> SessionId = erlang:md5(Name++Password++
integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:local_time()))),
								Pid ! {sessionserver, SessionId},
								spawn(fun()->autolog(SessionId) end),
								loop([{sessionserver,SessionId}|Database]);
		{logout, SessionId, Pid} -> 
			case lists:keymember(SessionId, 2, Database) of
				true -> Pid ! {ok, SessionId}, loop(lists:keydelete(SessionId,2, Database));
				_ -> Pid ! {error, SessionId}, loop(Database)
			end;
		{autologout, SessionId} -> 
			case lists:keymember(SessionId, 2, Database) of
				true -> loop(lists:keydelete(SessionId,2, Database));
				_ -> loop(Database)
			end;
		{validate, SessionId, Pid} ->
			Pid ! {SessionId, lists:keymember(SessionId, 2, Database)}, loop(Database)
end.

% autolog is Timer process that sends exit signal to Session
autolog(SessionId)->
receive 
after ?SESSION_TIMEOUT -> sessionserver ! {autologout, SessionId}
end.
