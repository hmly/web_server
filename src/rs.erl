-module('rs').

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() -> register(server, spawn (fun loop/0)).

loop ()->
    loop([]).

loop (Database)->
    receive
	{Client,{register, Name, Password}} ->			
	    handle_register(Client, Name, Password, Database);
	{Client,{unregister,Name,Password}} ->
	    handle_unregister(Client, Name, Password, Database);
	{Client,{login, Name, Password}} -> 
		handle_login(Client, Name, Password, Database);
	{Client,{logout, SessionId}} -> 
		handle_logout(Client, SessionId, Database);
	{Client,{store, SessionId, Key, Value}} -> 
		handle_store(Client, SessionId, Database, Key, Value);
	{Client,{lookup, SessionId, Key}} ->
		handle_lookup(Client, SessionId, Database, Key)
    end.

handle_register(Client, Name, Password, Database) ->
	case lists:keymember(Name,1,Database) of
		true -> Client ! {error,server,duplicate_name}, loop(Database);
		_    -> Len = try string:len(Password)
					catch
						_:_ -> Client ! {error,server,bad_credentials}, loop(Database)
					end,
					 case Len >= 8 of
						 true -> Client ! {ok,server,Name}, loop([{Name,Password}|Database]);
						 false -> Client ! {error,server,bad_credentials}, loop(Database)
					 end			
	    end.

% can use keydelete since names are unique, and password already matched
handle_unregister(Client, Name, Password, Database) ->
	case lists:keymember (Name,1,Database) of
		true -> case lists:keyfind(Name,1,Database) =:= {Name,Password} of
			    true -> Client ! {ok,server,Name},
				    loop (lists:keydelete(Name, 1, Database));
			    _    -> Client ! {error,server,bad_credentials},
				    loop (Database)
			end;
		_    -> Client ! {error,server,bad_credentials},
			loop(Database)
	    end.

handle_login(Client, Name, Password, Database) ->
	case lists:keyfind(Name,1,Database) of
		{Name, Password} -> sessionserver ! {login, Name, Password, sessionserver, self()},
							receive
								{sessionserver, SessionId} -> Client ! {ok, server, {sessionserver,SessionId}}
							end,
							loop(Database);
		_ -> Client ! {error, server, bad_credentials}, loop(Database)
	end.

handle_logout(Client, SessionId, Database) ->
	sessionserver ! {logout, SessionId, self()},
	receive
		{Status, SessionId} -> Client ! {Status, server, SessionId}
	end,
	loop(Database).

handle_store(Client, SessionId, Database, Key, Value)->
	sessionserver ! {validate, SessionId, self()},
	receive
		{SessionId, true} -> kvserver ! {self(), {store, Key, Value}},
			receive
				{kvserver, true} -> Client ! {ok, server, Key} 
			end;
		{SessionId, false} -> Client ! {error, server, bad_session}
	end,
	loop(Database).


handle_lookup(Client, SessionId, Database, Key) ->
	sessionserver ! {validate, SessionId, self()},
	receive
		{SessionId,true} -> kvserver ! {self(), {lookup, Key}},
				receive
					{kvserver, {ok, Value}} -> Client ! {ok, server, Value};
					{kvserver, undefined} -> Client ! {ok, server, "undefined"}
				end;
		{SessionId,false} -> Client ! {error, server, bad_session}
	end,
	loop(Database).

