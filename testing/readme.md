start with:
```
erl -pa /path/to/enacl/ebin
c(t1).
c(t2).
c(t3).
c(t4).
c(t5).
```

t1:run(Name)
generates box_keypair Name.pubkey and Name.seckey

t2:run(Seckey_filename, Pubkey_filename, Message_plain_filename)
tests encrypted and decrypting content of file Message_plain_filename

t3:run(Addressbook, Alice_name, Bob_name, Message_plain_filename)
same as t2:run but key files are found by looking up the name in the config
for example: `t3:run("addressbook.conf", "alice", "bob", "msg").`
an example config:
```
#{name => "alice",
  pubkey => "/path/to/alice.pubkey",
  seckey => "/path/to/alice.seckey",
  ip => "0.0.0.0",
  port => 42069}.
#{name => "bob",
  pubkey => "/path/to/bob.pubkey",
  seckey => "/path/to/bob.seckey",
  ip => "0.0.0.0",
  port => 5555}.
```

t4:run()
tests handshake protocol to establish a secret key channel
described here: https://github.com/orchid-hybrid/up/blob/master/protocol-a4.md

t5:run_alice(Port)
runs alice from t4 as a networked server on Port

t5:run_bob(Host, Port)
runs bob from t4 as a networked client, connecting to Host:Port
