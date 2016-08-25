-module(web).
%% API
-export([start/0]).

start() -> rs:start(), ss:start(), kvs:start(),
  {ok, Listen} = gen_tcp:listen(8000, [binary, {packet, 0},
    {reuseaddr, true},{active, true}]),
    seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket),
  seq_loop(Listen).

loop(Socket) -> 
receive
	{tcp, Socket, Bin} ->
		io:format("Binary = ~p~n", [Bin]),
		Str = binary_to_list(Bin),
		io:format("Unpacked = ~p~n",[Str]),
		msg_handler(Socket, Str);
	{tcp_closed, Socket} ->
		io:format("Socket closed~n")
end.
response(Str) ->
  B = iolist_to_binary(Str),
  iolist_to_binary(
    io_lib:fwrite(
      "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
      [size(B), B])).

msg_handler(Socket, Str) ->
	Msg= lists:nth(2,string:tokens(Str, " /")),
	io:format("Message = ~p~n", [Msg]),
	Task = string:tokens(Msg, "?"),
	Request = lists:nth(1, Task),
	case Request of
		"register" ->
			{Name, Password} = sep_info(lists:nth(2,Task)),
          case reg(Name, Password) of
			{error, bad_credentials} -> gen_tcp:send(Socket, response("bad_credentials"));
            {error,duplicate_name} -> gen_tcp:send(Socket, response("duplicate_name"));
            {ok,Name} -> gen_tcp:send(Socket, response("ok"))
          end, loop(Socket);

        "unregister" ->
          {Name, Password} = sep_info(lists:nth(2,Task)),
          case unreg(Name, Password) of
            {error,bad_credentials} -> gen_tcp:send(Socket, response("bad_credentials"));
            {ok,Name} -> gen_tcp:send(Socket, response("ok"))
          end, loop(Socket);

        "login" ->
          {Name, Password} = sep_info(lists:nth(2,Task)),
          case login(Name, Password) of
            {error,bad_credentials} -> gen_tcp:send(Socket, response("bad_credentials"));
            {ok,{sessionserver, SessionId}} -> gen_tcp:send(Socket, 
				iolist_to_binary(io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nSet-Cookie: CS401=~p",
         [SessionId]))),gen_tcp:send(Socket, response("ok")), io:format("SessionId: ~p~n", [SessionId])
          end, loop(Socket);

		%cookie will delete after browser is closed
        "logout" ->
			Cookie_string = lists:last(string:tokens(Str, "\r\n")),
			Cookie = try get_cookie(Cookie_string)
					 catch
						 _:_-> gen_tcp:send(Socket, response("bad_session")), loop(Socket)
					 end,
			case logout(Cookie) of
          		{ok,Cookie} ->  gen_tcp:send(Socket, response("ok"));
				{error, Cookie}-> gen_tcp:send(Socket, response("bad_session"))
          end, loop(Socket);

        "get" ->
			Cookie_string = lists:last(string:tokens(Str, "\r\n")),
			Cookie = try get_cookie(Cookie_string)
					 catch
						 _:_-> gen_tcp:send(Socket, response("bad_session")), loop(Socket)
					 end,
          Key = get_info(lists:nth(2,Task)),
		  case lookup(Cookie, Key) of
          {error, bad_session} -> gen_tcp:send(Socket, response("bad_session"));
          {ok,Value} -> gen_tcp:send(Socket, response("ok+"++Value))
          end, loop(Socket);

        "put" ->
			Cookie_string = lists:last(string:tokens(Str, "\r\n")),
			Cookie = try get_cookie(Cookie_string)
					 catch
						 _:_-> gen_tcp:send(Socket, response("bad_session")), loop(Socket)
					 end,
          {Key, Value} = sep_info(lists:nth(2,Task)),
          %% Get cookie
          case store(Cookie, Key, Value) of
            {error,bad_session} -> gen_tcp:send(Socket, response("bad_session"));
            {ok,Key} -> gen_tcp:send(Socket, response("ok"))
          end, loop(Socket);

        "stop" -> io:format("Stopped~n");
		
		_ -> loop(Socket) 
	%catch everything else
      end.

reg(Name, Password) -> rpc(server, {register, Name, Password}).
unreg(Name, Password) -> rpc(server, {unregister, Name, Password}).
login(Name, Password) -> rpc(server, {login, Name, Password}).
logout(SessionId) -> rpc(server, {logout, SessionId}).
store(SessionId, Key, Value) -> rpc(server, {store, SessionId, Key, Value}).
lookup(SessionId, Key) -> rpc(server, {lookup, SessionId, Key}).

rpc (Pid, Request) ->
  Pid ! {self (), Request},
  receive
    {Status,Pid,Response} -> {Status,Response}
  end.

sep_info(Str) ->
	[A, B] = string:tokens(Str, "&"),
	{get_info(A),get_info(B)}.

get_info(Str) ->
	[_, Info] = string:tokens(Str, "="), Info.
	
get_cookie(Cstr) ->
	CookieStr = lists:nth(2,string:tokens(Cstr, "<>")),
	NumList = string:tokens(CookieStr, ","),
	rec_str(NumList, []).

rec_str([H|T],IntList) ->
	{I,_} = string:to_integer(H),rec_str(T, [I|IntList]);
rec_str([], IntList) ->
	list_to_binary(lists:reverse(IntList)).
