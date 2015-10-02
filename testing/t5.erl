-module(t5).
-compile(export_all).


tcp_bridge(Sock, Proc) ->
    receive
        stop ->  gen_tcp:close(Sock);
        {tcp, _, Data} -> Proc ! Data;
        {tcp_closed, _} -> exit(done);
        Data -> gen_tcp:send(Sock, Data)
    end,
    tcp_bridge(Sock, Proc).

init_client(Host, Port) ->
    receive
        Proc ->
            {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 4}]),
            tcp_bridge(Sock, Proc)
    end.

init_server(Port, Alice) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
                                         {active, false},
                                         {reuseaddr, true},
                                         {packet, 4}]),
    listen_loop(Listen, Alice).

listen_loop(Listen, Alice_Proc) ->
    io:format("listening!~n"),
    {ok, Sock} = gen_tcp:accept(Listen),
    Bob = spawn(?MODULE, init_handle_client, [Sock]),
    gen_tcp:controlling_process(Sock, Bob),
    Bob ! {start, spawn(Alice_Proc)},
    listen_loop(Listen, Alice_Proc).

init_handle_client(Sock) ->
    receive
        {start, Alice} ->
            inet:setopts(Sock, [{active, true}]),
            Alice ! self(),
            tcp_bridge(Sock, Alice)
    end.

run_alice() ->
    Alice = fun() -> t4:run_alice() end,
    spawn(fun() -> init_server(5555, Alice) end).

run_bob() ->
    Fake_Alice = spawn(fun() -> init_client("localhost", 5555) end),
    spawn(fun() ->  t4:run_bob(Fake_Alice) end),
    Fake_Alice.
