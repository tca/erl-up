-module(t1).
-compile(export_all).

run(Name) ->
    Pubkey_filename = Name ++  ".pubkey",
    Seckey_filename = Name ++  ".seckey",
    #{ public := Pubk, secret := Seck} = enacl:box_keypair(),
    Pubk_enc = base64:encode(Pubk),
    Seck_enc = base64:encode(Seck),
    ok = file:write_file(Pubkey_filename, Pubk_enc),
    ok = file:write_file(Seckey_filename, Seck_enc),

    {ok, Pubk1_enc} = file:read_file(Pubkey_filename),
    {ok, Seck1_enc} = file:read_file(Seckey_filename),
    Pubk1 = base64:decode(Pubk1_enc),
    Seck1 = base64:decode(Seck1_enc),

    Pubk = Pubk1,
    Seck = Seck1,
    
    io:format("ok\n").
    
