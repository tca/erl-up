-module(t2).
-compile(export_all).

run(Seckey_filename, Pubkey_filename, Message_plain_filename) ->

    {ok, Pubk1_enc} = file:read_file(Pubkey_filename),
    {ok, Seck1_enc} = file:read_file(Seckey_filename),
    Pubk1 = base64:decode(Pubk1_enc),
    Seck1 = base64:decode(Seck1_enc),

    {ok, Message_plain} = file:read_file(Message_plain_filename),
    io:format("~s\n", [Message_plain]),

    Nonce = crypto:rand_bytes(enacl:box_nonce_size()),
    Shared_key = enacl:box_beforenm(Pubk1, Seck1),

    Message_enc = enacl:box_afternm(Message_plain, Nonce, Shared_key),
    io:format("~s\n\n", [Message_enc]),

    {ok, Message_plain1} = enacl:box_open_afternm(Message_enc, Nonce, Shared_key),
    io:format("~s\n", [Message_plain1]),
    Message_plain = Message_plain1,
    io:format("ok\n").
