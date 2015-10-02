-module(t4).
-compile(export_all).

bin_bxor(Bin1, Bin2) ->
    Sz1 = size(Bin1)*8,
    Sz2 = size(Bin2)*8,
    <<Int1:Sz1>> = Bin1,
    <<Int2:Sz2>> = Bin2,
    Int3 = Int1 bxor Int2,
    Sz3 = max(Sz1, Sz2),
    <<Int3:Sz3>>.

run_alice() ->
    receive
        Bob -> 
            #{ public := EPubk, secret := ESeck} = enacl:box_keypair(),
            Bob ! EPubk,
            run_alice1(Bob, ESeck)
    end.

run_bob(Alice) ->
    AUTH_KEY_SIZE = enacl:auth_key_size(),
    BOX_NONCE_SIZE = enacl:box_nonce_size(),

    Alice ! self(),
    receive
        Alice_EPubk ->
            #{ public := EPubk, secret := ESeck} = enacl:box_keypair(),
            Nonce = enacl:randombytes(enacl:box_nonce_size()),
            Bob_Secret = enacl:randombytes(enacl:secretbox_key_size()),
            Bob_Commit_Key = enacl:randombytes(8),
            Bob_Commit = enacl:hash(<<EPubk/binary, Bob_Secret/binary, Bob_Commit_Key/binary>>),
            Bob_Commit_Enc = enacl:box(Bob_Commit, Nonce, Alice_EPubk, ESeck),
            Bob_Commit_Enc_Size= size(Bob_Commit_Enc),
            Package = <<EPubk:AUTH_KEY_SIZE/binary,
                        Nonce:BOX_NONCE_SIZE/binary,
                        Bob_Commit_Enc_Size:32,
                        Bob_Commit_Enc:Bob_Commit_Enc_Size/binary>>,
            Alice ! Package,
            run_bob1(Alice, Alice_EPubk, ESeck, Bob_Secret, Bob_Commit_Key, Nonce)
    end.

run_alice1(Bob, ESeck) ->
    AUTH_KEY_SIZE = enacl:auth_key_size(),
    BOX_NONCE_SIZE = enacl:box_nonce_size(),
    SECRETBOX_KEY_SIZE = enacl:secretbox_key_size(),
    SECRETBOX_NONCE_SIZE = enacl:secretbox_nonce_size(),
    receive
        <<Bob_EPubk:AUTH_KEY_SIZE/binary,
          Nonce:BOX_NONCE_SIZE/binary,
          Bob_Commit_Enc_Size:32,
          Bob_Commit_Enc:Bob_Commit_Enc_Size/binary>> ->
            {ok, Bob_Commit} = enacl:box_open(Bob_Commit_Enc, Nonce, Bob_EPubk, ESeck),
            Nonce1 = enacl:randombytes(enacl:box_nonce_size()),
            Alice_Secret = enacl:randombytes(enacl:secretbox_key_size()),
            Sec_Nonce = enacl:randombytes(enacl:secretbox_nonce_size()),
            Package = <<Alice_Secret:SECRETBOX_KEY_SIZE/binary,
                        Sec_Nonce:SECRETBOX_NONCE_SIZE/binary,
                        Nonce1:BOX_NONCE_SIZE/binary>>,
            Package_Enc = enacl:box(Package, Nonce, Bob_EPubk, ESeck),
            Bob ! Package_Enc,
            run_alice2(Bob, ESeck, Bob_EPubk, Bob_Commit, Alice_Secret, Sec_Nonce, Nonce1)
    end.

run_bob1(Alice, Alice_EPubk, ESeck, Bob_Secret, Bob_Commit_Key, Nonce) ->
    BOX_NONCE_SIZE = enacl:box_nonce_size(),
    SECRETBOX_KEY_SIZE = enacl:secretbox_key_size(),
    SECRETBOX_NONCE_SIZE = enacl:secretbox_nonce_size(),
    receive
        Package_Enc ->
            {ok, Package} = enacl:box_open(Package_Enc, Nonce, Alice_EPubk, ESeck),
            <<Alice_Secret:SECRETBOX_KEY_SIZE/binary,
              Sec_Nonce:SECRETBOX_NONCE_SIZE/binary,
              Nonce1:BOX_NONCE_SIZE/binary>> = Package,
            Bob_Commit_Key_Size = size(Bob_Commit_Key),
            Sec_Nonce1 = enacl:randombytes(enacl:secretbox_nonce_size()),
            Package1 = <<Bob_Secret:SECRETBOX_KEY_SIZE/binary,
                         Sec_Nonce1:SECRETBOX_NONCE_SIZE/binary,
                         Bob_Commit_Key_Size:32,
                         Bob_Commit_Key:Bob_Commit_Key_Size/binary>>,
            Package1_Enc = enacl:box(Package1, Nonce1, Alice_EPubk, ESeck),
            Alice ! Package1_Enc,

            Secret = bin_bxor(Alice_Secret, Bob_Secret),
            run_bob2(Secret, Sec_Nonce, Bob_Commit_Key)
    end.

run_alice2(Bob, ESeck, Bob_EPubk, Bob_Commit, Alice_Secret, Sec_Nonce, Nonce1) ->
    SECRETBOX_KEY_SIZE = enacl:secretbox_key_size(),
    SECRETBOX_NONCE_SIZE = enacl:secretbox_nonce_size(),
    receive
        Package1_Enc ->
            {ok, Package1} = enacl:box_open(Package1_Enc, Nonce1, Bob_EPubk, ESeck),
            <<Bob_Secret:SECRETBOX_KEY_SIZE/binary,
              Sec_Nonce1:SECRETBOX_NONCE_SIZE/binary,
              Bob_Commit_Key_Size:32,
              Bob_Commit_Key:Bob_Commit_Key_Size/binary>> = Package1,
            Bob_Commit = enacl:hash(<<Bob_EPubk/binary, Bob_Secret/binary, Bob_Commit_Key/binary>>),
            Secret = bin_bxor(Alice_Secret, Bob_Secret),

            Sec_Nonce2 = enacl:randombytes(enacl:secretbox_nonce_size()),
            Msg = <<Bob_Commit_Key:8/binary, Sec_Nonce2:SECRETBOX_NONCE_SIZE/binary>>,
            Msg_Enc = enacl:secretbox(Msg, Sec_Nonce, Secret),
            Bob ! Msg_Enc,
            exit(done)
    end.

run_bob2(Secret, Sec_Nonce, Bob_Commit_Key) ->
    SECRETBOX_NONCE_SIZE = enacl:secretbox_nonce_size(),
    receive
        Msg_Enc ->
            {ok, Msg} = enacl:secretbox_open(Msg_Enc, Sec_Nonce, Secret),
            <<Bob_Commit_Key:8/binary, Sec_Nonce2:SECRETBOX_NONCE_SIZE/binary>> = Msg,
            io:format("ok!~n")
    end,
    exit(done).

run() ->
    Alice = spawn(fun() -> run_alice() end),
    spawn(fun() -> run_bob(Alice) end).
