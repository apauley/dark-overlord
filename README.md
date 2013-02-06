How can we demo distributed Erlang if not by having a dark overlord control an army of minions...

## Overview

Only the dark overlord needs to have this code compiled and loaded into an Erlang VM.
The minions only needs to connect to the master node.
The necessary minion code will then be loaded into the remote nodes by the master node.

### Compiling

```bash
$ ./rebar get-deps update-deps compile generate
```

## The Dark Overlord: Starting the master node

```bash
$ ./rel/overlord/bin/overlord console
```

## The Minions: Connect to the Dark Overlord

Each minion should be able to ping the name overlord, so make sure that the minion /etc/hosts files are updated with the name "overlord" and the corresponding IP address:

```bash
$ cat /etc/hosts
127.0.0.1	localhost
192.168.8.123	overlord
```

```bash
$ ping overlord
PING overlord (192.168.8.123): 56 data bytes
64 bytes from 192.168.8.123: icmp_seq=0 ttl=64 time=0.044 ms
^C
--- overlord ping statistics ---
1 packets transmitted, 1 packets received, 0.0% packet loss
round-trip min/avg/max/stddev = 0.044/0.044/0.044/0.000 ms
```

Once the above is working you can connect to the master node:
```bash
$ erl -sname minion -connect_all false -setcookie TheHypnoSpongeKnowsBest -s net_adm ping_list hypnosponge@overlord
```

And here are some shell commands to check if it worked, you should see your overlord in the list of nodes:
```erlang
(minion@iris)1> nodes().
[hypnosponge@overlord]
(minion@iris)2> net_adm:ping('hypnosponge@overlord').
pong
```

## The Dark Overlord: Controlling your army of minions

Once your minions have connected, you can send some direct commands (rpc calls):

```erlang
(hypnosponge@overlord)1> darklord:minion_info().
(hypnosponge@overlord)2> darklord:sing().
```

Or you can start up the hypnosponge and compell your minions to do your bidding out "their own free will" (muhahaha!!!):
```erlang
(hypnosponge@overlord)3> sponge:start().
(hypnosponge@overlord)4> sponge:minion_info().
(hypnosponge@overlord)5> sponge:sing().
```

The hypnosponge spawns process on the remote nodes.
The idea with the hypnosponge is that the minions are instructed to do something and to continue with this on their own.
With the darklord rpc calls above we needed to wait for the calls to finish.
