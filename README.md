How can we demo distributed Erlang if not by having a dark overlord control an army of minions...

## The Dark Overlord: Starting the master node

```bash
$ erl -sname hypnosponge@overlord -setcookie TheHypnoSpongeKnowsBest
```

Once your minions have connected, you can send some direct commands:

```erlang
(hypnosponge@overlord)1> darklord:sing().
```

Or you can start up the hypnosponge and compell your minions to do your bidding out "their own free will" (muhahaha!!!):
```erlang
(hypnosponge@overlord)2> sponge:start().
(hypnosponge@overlord)3> sponge:sing().
```

## The Minions: Connect to the dark overlord

```bash
$ erl -sname minion -connect_all false -setcookie TheHypnoSpongeKnowsBest -s net_adm ping_list hypnosponge@overlord
```

```erlang
(minion@iris)1> nodes().
[hypnosponge@overlord]
(minion@iris)2> net_adm:ping('hypnosponge@overlord').
pong
```
