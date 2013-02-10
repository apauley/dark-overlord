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

## The Dark Overlord: Starting the master nodes

We will be starting two master nodes (preferably on different hardware).
One will be the overlord, the other his evil twin.
If something should happen to our overlord (the horror!), his evil twin would be willing to take his place.

Edit rel/files/vm.args (on both computers) and make sure that the IP address is set correctly so that the minions can reach you.

On the overlord computer this might look like:
```
-name overlord@192.168.8.123
```

And on the evil twin computer we may have:
```
-name eviltwin@192.168.8.124
```

Edit rel/files/sys.config and make sure the references to the nodes there are the same as the above.
Our sys.config needs to be identical on both computers.

Then generate a release (same command on both computers):
```bash
$ ./rebar generate
```

Among other things this will generate a new vm.args in rel/overlord/releases/1/vm.args based on the edited one above.

Now we can start an Erlang shell with our apps loaded.
Both master nodes have to be started at roughly the same time.

Among other things this will start the hypnosponge.
As you might very well know, any decent overlord needs a hypnosponge for Minion Mind Control (MMC).

The overlord output may look like this:
```erlang
$ ./rel/overlord/bin/overlord console
Exec: /dark-overlord/rel/overlord/erts-5.9.3.1/bin/erlexec -boot /dark-overlord/rel/overlord/releases/1/overlord -mode embedded -config /dark-overlord/rel/overlord/releases/1/sys.config -args_file /dark-overlord/rel/overlord/releases/1/vm.args -- console
Root: /dark-overlord/rel/overlord
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

22:35:11.190 overlord@192.168.8.123 [overlord_app] <0.57.0> || Starting app: normal
22:35:11.190 overlord@192.168.8.123 [hypnosponge_sup] <0.58.0> || Hello from the hypnosponge supervisor
22:35:11.190 overlord@192.168.8.123 [hypnosponge] <0.59.0> || Hello from the hypnosponge itself!
22:35:11.191 overlord@192.168.8.123 [minion_supersup] <0.60.0> || Hello from the minion supersup
22:35:11.191 overlord@192.168.8.123 [hypnosponge] <0.59.0> || The minion supersup (<0.60.0>) has been attached to hypnosponge_sup (<0.58.0>)
22:35:11.191 overlord@192.168.8.123 [hypnosponge] <0.59.0> || hypnosponge_sup (<0.58.0>) now has 2 children: [<0.60.0>,
                                                                                                           <0.59.0>]
22:35:11.191 overlord@192.168.8.123 [hypnosponge] <0.61.0> || Hello from your minion_recruiter
Eshell V5.9.3.1  (abort with ^G)
(overlord@192.168.8.123)1> application:which_applications().
[{overlord,"A way to demo Erlang/OTP distributed features by turning the laptops of your audience into minions.",
           "0.1.0"},
 {sudoku,"An Erlang implementation of Norvig's Sudoku solver",
         "0.9.0"},
 {sasl,"SASL  CXC 138 11","2.2.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.3"},
 {kernel,"ERTS  CXC 138 10","2.15.3"}]
(overlord@192.168.8.123)2> 
```

His evil twin:
```erlang
$ ./rel/overlord/bin/overlord console
Exec: /dark-overlord/rel/overlord/erts-5.9.3.1/bin/erlexec -boot /dark-overlord/rel/overlord/releases/1/overlord -mode embedded -config /dark-overlord/rel/overlord/releases/1/sys.config -args_file /dark-overlord/rel/overlord/releases/1/vm.args -- console
Root: /dark-overlord/rel/overlord
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
(eviltwin@192.168.8.124)1> application:which_applications().
[{sudoku,"An Erlang implementation of Norvig's Sudoku solver",
         "0.9.0"},
 {sasl,"SASL  CXC 138 11","2.2.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.3"},
 {kernel,"ERTS  CXC 138 10","2.15.3"}]
(eviltwin@192.168.8.124)2> 
```

Note that the overlord app is not running on eviltwin. This will change if the overlord node goes down.

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

Once the above is working you can connect to the overlord node:
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

Once your minions start connecting, you will see output similar to the following in your console:
```
overlord@10.0.0.103 [hypnosponge] <0.61.0> || Enslaved node 'minion@10.0.0.101' (remote minion supervisor is <20972.48.0>)
22:37:43.393 overlord@10.0.0.103 [hypnosponge] <0.61.0> || minion_supersup (<0.60.0>) now has 1 children: [<20972.48.0>]
22:37:43.465 overlord@10.0.0.103 [hypnosponge] <0.59.0> || Minion <20972.50.0> on minion@10.0.0.101 reporting for duty
```

Now you can compell your minions to do your bidding out of "their own free will", muhahaha!!!

```erlang
(overlord@192.168.8.123)3> overlord:minion_nodes().
(overlord@192.168.8.123)4> overlord:minion_info().
(overlord@192.168.8.123)5> overlord:sing().
```

Our overlord needs his minions to do some very important work for him, like solving Sudoku puzzles.
Once you start this, the minions will have a huge CPU load. Not that we care about them...

```erlang
(overlord@192.168.8.123)6> overlord:sudoku_start().
(overlord@192.168.8.123)7> overlord:sudoku_stats(). %% Do this a few times
```

While the minions are hard at work, we can see what happens if they crash:
```erlang
(overlord@192.168.8.123)8> overlord:minion_crash().
```

And to stop the madness:
```erlang
(overlord@192.168.8.123)9> overlord:sudoku_stop().
```

We can also see what happens when our hypnosponge crashes:
```erlang
(overlord@192.168.8.123)10> overlord:sponge_crash().
```

Have a look inside overlord.erl for some more exported functions.

Finally, we need to test what happens if an entire node goes down.
Kill the overlord node in some ungraceful way (I do ctrl-g followed by q).

The app should failover to the evil twin, and you will be able to see the startup output on the node.

You can bring back the overlord node once you have played a bit on the evil twin.
The overlord node should do a takeover, and the app will be stopped on the evil twin node.
