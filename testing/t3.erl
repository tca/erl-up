-module(t3).
-compile(export_all).

lookup_name(Name, List) ->
    case List of
        [] -> error;
        [First | Rest] ->
            case First of
                #{name := Name} -> {ok, First};
                _Else -> lookup_name(Name, Rest)
            end
    end.

read_config(Filename) ->
    {ok, Config} = file:consult(Filename),
    Config.

write_config(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).

run(Addressbook, Alice_name, Bob_name, Message_plain_filename) ->
    Config = read_config(Addressbook),
    {ok, #{ seckey := Alice_seckey }} = lookup_name(Alice_name, Config),
    {ok, #{ pubkey := Bob_pubkey }} = lookup_name(Bob_name, Config),
    t2:run(Alice_seckey, Bob_pubkey, Message_plain_filename).
