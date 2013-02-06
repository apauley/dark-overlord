How can we demo distributed Erlang if not by having a dark overlord control an army of minions...

## Overview

This code is intended to be used in a setup where one person (the dark overlord)
demonstrates some cool Erlang/OTP features to a group of people with computers
on the same network.

Only the dark overlord needs to have this code compiled and loaded into an Erlang VM.
The minions only need to connect to the master node.
The necessary minion code will then be loaded into the remote nodes by the master node.

### Compiling

```bash
$ ./rebar get-deps update-deps compile
```

## The Dark Overlord: Starting the master node

Edit rel/files/vm.args and make sure that the IP address is set correctly so that the minions can reach you:
```
-name overlord@192.168.8.123
```

Then generate a release:
```bash
$ ./rebar generate
```

Among other things this will generate a new vm.args in rel/overlord/releases/1/vm.args based on the edited one above.

Now we can start an Erlang shell with our apps loaded:
```bash
$ ./rel/overlord/bin/overlord console
Exec: /Users/andreas/Projekte/Persoonlik/Erlang/dark-overlord/rel/overlord/erts-5.9.3.1/bin/erlexec -boot /Users/andreas/Projekte/Persoonlik/Erlang/dark-overlord/rel/overlord/releases/1/overlord -mode embedded -config /Users/andreas/Projekte/Persoonlik/Erlang/dark-overlord/rel/overlord/releases/1/sys.config -args_file /Users/andreas/Projekte/Persoonlik/Erlang/dark-overlord/rel/overlord/releases/1/vm.args -- console
Root: /Users/andreas/Projekte/Persoonlik/Erlang/dark-overlord/rel/overlord
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
(overlord@10.0.0.101)1> application:which_applications().
[{overlord,"A way to demo Erlang/OTP distributed features by turning the laptops of your audience into minions.",
           "0.1.0"},
 {sudoku,"An Erlang implementation of Norvig's Sudoku solver",
         "0.9.0"},
 {sasl,"SASL  CXC 138 11","2.2.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.3"},
 {kernel,"ERTS  CXC 138 10","2.15.3"}]
(overlord@10.0.0.101)2> 
```

## The Minions: Connect to the Dark Overlord

We are starting our VM's using fully qualified names (the -name option as opposed to -sname),
so we can use the IP address of the overlord. All connected VM's need to use the same naming type.

If you want to change all references in this demo to short names you might have to ensure that the overlord IP
is added to the hosts file of each minion computer, and then use that name instead of the IP address.

Each minion should be able to ping the overlord:

```bash
$ ping -c 1 192.168.8.123
PING 192.168.8.123 (192.168.8.123): 56 data bytes
64 bytes from 192.168.8.123: icmp_seq=0 ttl=64 time=0.110 ms

--- 192.168.8.123 ping statistics ---
1 packets transmitted, 1 packets received, 0.0% packet loss
round-trip min/avg/max/stddev = 0.110/0.110/0.110/0.000 ms
```

Once the above is working you can connect to the master node:
```bash
$ erl -name minion -setcookie overlord -s net_adm ping_list overlord@192.168.8.123
```

You may need to change "-name minion" above to include the minion IP, as in "-name minion@192.168.8.111".

You can enter some shell commands to check if it worked. You should see your overlord in the list of nodes:
```erlang
(minion@iris)1> nodes().
[overlord@192.168.8.123]
(minion@iris)2> net_adm:ping('overlord@192.168.8.123').
pong
```

## The Dark Overlord: Controlling your army of minions

Once your minions have connected, you can send some direct commands (rpc calls):

```erlang
(overlord@192.168.8.123)1> darklord:minion_info().
(overlord@192.168.8.123)2> darklord:sing().
```

Or you can start up the hypnosponge and compell your minions to do your bidding out of "their own free will" (muhahaha!!!):
```erlang
(overlord@192.168.8.123)3> sponge:start().
(overlord@192.168.8.123)4> sponge:minion_info().
(overlord@192.168.8.123)5> sponge:sing().
```

The hypnosponge spawns process on the remote nodes.
The idea with the hypnosponge is that the minions are instructed to do something and to continue with this on their own.
With the darklord rpc calls above we needed to wait for the calls to finish.
