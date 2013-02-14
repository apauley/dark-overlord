# Overview

Welcome, dark overlord!

Here within you will find everything needed to
enslave your army of minions!

Our most important tool is the hypnosponge.
As you might very well know, any decent overlord needs a hypnosponge for Minion Mind Control (MMC).

With the use of your trusted hypnosponge you will be able to have
every enslaved minion do your bidding "out of their own free will".

![Startup sequence of our dark overlord app](https://raw.github.com/apauley/dark-overlord/master/start_sequence.jpg "Startup sequence for Supervisors and Workers")

This code is intended to be used in a setup where one person (the dark overlord)
demonstrates some cool Erlang/OTP features to a group of people with computers
on the same network.

I have only tested this on Erlang R15B03-1, older versions like R14B may or may not work as expected.

# Getting up and running

The dark overlord needs to have this code compiled and loaded into an Erlang VM.
The minions only need to connect to the overlord node.
The necessary minion code will then be loaded into the remote nodes by the overlord node.

## The Dark Overlord: Compiling the Code

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
-name overlord@10.1.1.1
```

And on the evil twin computer we may have:
```
-name eviltwin@10.1.1.2
```

Edit rel/files/sys.config and make sure the references to the nodes there are the same as the above.
Our sys.config needs to be identical on both computers.

Then generate a release (same command on both computers):
```bash
$ ./rebar generate
```

Now we can start an Erlang shell with our apps loaded.
Both master nodes have to be started at roughly the same time.

The overlord output may look like this:
```erlang-repl
$ ./rel/overlord/bin/overlord console
Exec: /dark-overlord/rel/overlord/erts-5.9.3.1/bin/erlexec -boot /dark-overlord/rel/overlord/releases/1/overlord -mode embedded -config /dark-overlord/rel/overlord/releases/1/sys.config -args_file /dark-overlord/rel/overlord/releases/1/vm.args -- console
Root: /dark-overlord/rel/overlord
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

22:35:11.190 overlord@10.1.1.1 [overlord_app] <0.57.0> || Starting app: normal
22:35:11.190 overlord@10.1.1.1 [hypnosponge_sup] <0.58.0> || Hello from the hypnosponge supervisor
22:35:11.190 overlord@10.1.1.1 [hypnosponge] <0.59.0> || Hello from the hypnosponge itself!
22:35:11.191 overlord@10.1.1.1 [minion_supersup] <0.60.0> || Hello from the minion supersup
22:35:11.191 overlord@10.1.1.1 [hypnosponge] <0.59.0> || The minion supersup (<0.60.0>) has been attached to hypnosponge_sup (<0.58.0>)
22:35:11.191 overlord@10.1.1.1 [hypnosponge] <0.59.0> || hypnosponge_sup (<0.58.0>) now has 2 children: [<0.60.0>,
                                                                                                           <0.59.0>]
22:35:11.191 overlord@10.1.1.1 [hypnosponge] <0.61.0> || Hello from your minion_recruiter
Eshell V5.9.3.1  (abort with ^G)
(overlord@10.1.1.1)1> application:which_applications().
[{overlord,"A way to demo Erlang/OTP distributed features by turning the laptops of your audience into minions.",
           "0.1.0"},
 {sudoku,"An Erlang implementation of Norvig's Sudoku solver",
         "0.9.0"},
 {sasl,"SASL  CXC 138 11","2.2.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.3"},
 {kernel,"ERTS  CXC 138 10","2.15.3"}]
(overlord@10.1.1.1)2> 
```

His evil twin:
```erlang-repl
$ ./rel/overlord/bin/overlord console
Exec: /dark-overlord/rel/overlord/erts-5.9.3.1/bin/erlexec -boot /dark-overlord/rel/overlord/releases/1/overlord -mode embedded -config /dark-overlord/rel/overlord/releases/1/sys.config -args_file /dark-overlord/rel/overlord/releases/1/vm.args -- console
Root: /dark-overlord/rel/overlord
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
(eviltwin@10.1.1.2)1> application:which_applications().
[{sudoku,"An Erlang implementation of Norvig's Sudoku solver",
         "0.9.0"},
 {sasl,"SASL  CXC 138 11","2.2.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.3"},
 {kernel,"ERTS  CXC 138 10","2.15.3"}]
(eviltwin@10.1.1.2)2> 
```

Note that the overlord app is not running on eviltwin. This will change if the overlord node goes down.

## The Minions: Connect to the Dark Overlord

We are starting our VM's using fully qualified names (the -name option as opposed to -sname),
so we can use the IP address of the overlord. All connected VM's need to use the same naming type.

If you want to change all references in this demo to short names you might have to ensure that the overlord IP
is added to the hosts file of each minion computer, and then use that name instead of the IP address.

Each minion should be able to ping the overlord:

```bash
$ ping -c 1 10.1.1.1
PING 10.1.1.1 (10.1.1.1): 56 data bytes
64 bytes from 10.1.1.1: icmp_seq=0 ttl=64 time=0.110 ms

--- 10.1.1.1 ping statistics ---
1 packets transmitted, 1 packets received, 0.0% packet loss
round-trip min/avg/max/stddev = 0.110/0.110/0.110/0.000 ms
```

Once the above is working you can connect to the overlord node (change the IP addresses of the minion and overlord as appropriate):
```bash
$ erl -name minion@127.0.0.1 -setcookie overlord -s net_adm ping_list overlord@10.1.1.1
```

You can enter some shell commands to check if it worked. You should see your overlord in the list of nodes:

```erlang-repl
(minion@iris)1> nodes().
[overlord@10.1.1.1]
(minion@iris)2> net_adm:ping('overlord@10.1.1.1').
pong
```

## The Dark Overlord: Controlling your army of minions

Once your minions start connecting, you will see output similar to the following in your console:
```
overlord@10.1.1.1 [hypnosponge] <0.61.0> || Enslaved node 'minion@10.1.1.101' (remote minion supervisor is <20972.48.0>)
22:37:43.393 overlord@10.1.1.1 [hypnosponge] <0.61.0> || minion_supersup (<0.60.0>) now has 1 children: [<20972.48.0>]
22:37:43.465 overlord@10.1.1.1 [hypnosponge] <0.59.0> || Minion <20972.50.0> on minion@10.1.1.101 reporting for duty
```

Now you can use the exported API functions in the overlord module to
send commands to your minions, for example:

```erlang-repl
(overlord@10.1.1.1)3> overlord:minion_nodes().
(overlord@10.1.1.1)4> overlord:minion_info().
(overlord@10.1.1.1)5> overlord:sing().
```

Our overlord needs his minions to do some very important work for him, like solving Sudoku puzzles.
Once you start this, the minions will have a huge CPU load. Not that
we care much about them...

```erlang-repl
(overlord@10.1.1.1)6> overlord:sudoku_start().
(overlord@10.1.1.1)7> overlord:sudoku_stats(). %% Do this a few times
```

While the minions are hard at work, we can see what happens if they crash:
```erlang-repl
(overlord@10.1.1.1)8> overlord:minion_crash().
```

And to stop the madness:
```erlang-repl
(overlord@10.1.1.1)9> overlord:sudoku_stop().
```

We can also see what happens when our hypnosponge crashes:
```erlang-repl
(overlord@10.1.1.1)10> overlord:sponge_crash().
```

Have a look inside overlord.erl for some more exported functions.

Finally, we need to test what happens if an entire node goes down.
Kill the overlord node in some ungraceful way (I do ctrl-g followed by q).

The app should failover to the evil twin, and you will be able to see the startup output on the node.

You can bring back the overlord node once you have played a bit on the evil twin.
The overlord node should do a takeover, and the app will be stopped on the evil twin node.

# Inside the Code

> &ldquo;If a listener nods his head when you're explaining your program, wake him up.&rdquo;
>
> &mdash; <cite>Alan Perlis</cite>

## App Startup

Our code is started using the following general order:
Release -> Applications -> Supervisors -> Workers

The release ensures that all applications are started at boot time.
One if these is our overlord application.

Have a look at the picture of our app startup sequence above for a
visual representation of this section.

The overlord application starts the *hypnosponge supervisor*.
The *hypnosponge supervisor* starts the first worker, our *hypnosponge*.

The first thing that the *hypnosponge* does after starting up is to
ask the *hypnosponge supervisor* to start another child, the *minion
supersup*.
Thy *hypnosponge* links itself to the *minion supersup*, so that in
the event that one of them dies, they will both be restarted by the
*hypnosponge supervisor*.

The *hypnosponge* also spawns a *minion recruiter*, responsible for detecting and
enslaving new minions. The *minion recruiter* and the *hypnosponge*
are linked, so that in the event that one of them dies, they will
both be restarted by the *hypnosponge supervisor*.

The *hypnosponge supervisor* is now responsible for monitoring 2
processes:
 * the *hypnosponge*
 * the *minion supersup*

The *minion supersup* is running on the overlord node. It has the
responsibility of starting and monitoring the supervisors on the remote minion nodes.
As soon as the *minion recruiter* detects a newly connected node, it
will instruct the *minion supersup* to start a *minion supervisor* on
the remote node.

The *minion supervisor* runs on the remote minion node.
It immediately starts and monitors the *minion*, which will do the
actual work.

## When processes die: a guide to the afterlife

If our *minion* dies for any reason, the *minion
supervisor* is tasked to respawn it.

If the *minion supervisor* dies, then the *minion supersup* will know
about this and do whatever we configured it to do. Because this link
is between different nodes over a network, it could be that the
*minion supersup* gets notified because the entire node went down.
If this is the case we do not want our *minion supersup* to make
countless futile attempts at restarting the remote *minion
supervisor*.
Instead, the *minion supersup* will do nothing and just wait for the
*minion recruiter* to detect that the node is up and available for
enslavement.

The *hypnosponge supervisor* will restart any of the following
processes, should they die on us:
 * The *minion supersup*
 * The *hypnosponge*

If the *hypnosponge supervisor* dies the entire overlord node will
crash.
We can do this by sending a kill signal to the process ID:
```erlang-repl
(overlord@10.1.1.1)11> Sup = pid(0,58,0).
<0.58.0>
(overlord@10.1.1.1)12> exit(Sup, kill).

=ERROR REPORT==== 11-Feb-2013::12:16:49 ===
** Generic server minion_supersup terminating 
** Last message in was {'EXIT',<0.58.0>,killed}
** When Server state == {state,
                            {local,minion_supersup},
                            simple_one_for_one,
                            [{child,undefined,minion_makeshift_sup,
                                 {minion_makeshift_sup,start_link,[]},
                                 temporary,5000,worker,
                                 [minion_makeshift_sup]}],
                            undefined,1,3,[],minion_supersup,[]}
** Reason for termination == 
** killed
true
(overlord@10.1.1.1)13> 
=INFO REPORT==== 11-Feb-2013::12:16:49 ===
    application: overlord
    exited: killed
    type: permanent

{"Kernel pid terminated",application_controller,"{application_terminated,overlord,killed}"}

Crash dump was written to: erl_crash.dump
Kernel pid terminated (application_controller) ({application_terminated,overlord,killed})
```

If the *overlord_app* dies the overlord node will also crash:
```erlang-repl
(overlord@10.1.1.1)14> App = pid(0,57,0).
<0.57.0>
(overlord@10.1.1.1)15> exit(App, kill).
true
(overlord@10.1.1.1)16> 
=INFO REPORT==== 11-Feb-2013::12:30:33 ===
    application: overlord
    exited: killed
    type: permanent
(overlord@10.1.1.1)17> {"Kernel pid terminated",application_controller,"{application_terminated,overlord,killed}"}

Crash dump was written to: erl_crash.dump
Kernel pid terminated (application_controller) ({application_terminated,overlord,killed})
```

In the two cases above I actually want the evil twin to
automatically take over. This does not currently happen. Some
investigation is in order...

What does work quite nicely however, is when the Erlang VM is killed.
```bash
$ kill -9 25561
```

```erlang-repl
(overlord@10.1.1.1)18> Killed: 9
```

Now the failover happens to the evil twin within our configured 5 seconds:
```erlang-repl
(eviltwin@10.1.1.2)1> 12:38:48.744 eviltwin@10.1.1.2 [overlord_app] <0.80.0> || Starting app: normal
12:38:48.745 eviltwin@10.1.1.2 [hypnosponge_sup] <0.81.0> || Hello from the hypnosponge supervisor
12:38:48.745 eviltwin@10.1.1.2 [hypnosponge] <0.82.0> || Hello from the hypnosponge itself!
12:38:48.745 eviltwin@10.1.1.2 [minion_supersup] <0.83.0> || Hello from the minion supersup
12:38:48.745 eviltwin@10.1.1.2 [hypnosponge] <0.82.0> || The minion supersup (<0.83.0>) has been attached to hypnosponge_sup (<0.81.0>)
12:38:48.745 eviltwin@10.1.1.2 [hypnosponge] <0.82.0> || hypnosponge_sup (<0.81.0>) now has 2 children: [<0.83.0>,
                                                                                                          <0.82.0>]
12:38:48.745 eviltwin@10.1.1.2 [hypnosponge] <0.84.0> || Hello from your minion_recruiter

(eviltwin@10.1.1.2)1> 
```

As soon as the overlord node is started again it will take over:
```erlang-repl
$ ./rel/overlord/bin/overlord console
Exec: /dark-overlord/rel/overlord/erts-5.9.3.1/bin/erlexec -boot /dark-overlord/rel/overlord/releases/1/overlord -mode embedded -config /dark-overlord/rel/overlord/releases/1/sys.config -args_file /dark-overlord/rel/overlord/releases/1/vm.args -- console
Root: /dark-overlord/rel/overlord
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

12:46:40.598 overlord@10.1.1.1 [overlord_app] <0.56.0> || Taking over from 'eviltwin@10.1.1.2'
12:46:40.598 overlord@10.1.1.1 [hypnosponge_sup] <0.57.0> || Hello from the hypnosponge supervisor
12:46:40.598 overlord@10.1.1.1 [hypnosponge] <0.58.0> || Hello from the hypnosponge itself!
12:46:40.599 overlord@10.1.1.1 [minion_supersup] <0.59.0> || Hello from the minion supersup
12:46:40.599 overlord@10.1.1.1 [hypnosponge] <0.58.0> || The minion supersup (<0.59.0>) has been attached to hypnosponge_sup (<0.57.0>)
12:46:40.599 overlord@10.1.1.1 [hypnosponge] <0.58.0> || hypnosponge_sup (<0.57.0>) now has 2 children: [<0.59.0>,
                                                                                                          <0.58.0>]
12:46:40.599 overlord@10.1.1.1 [hypnosponge] <0.60.0> || Hello from your minion_recruiter
Eshell V5.9.3.1  (abort with ^G)
(overlord@10.1.1.1)1> 
```

On the eviltwin node we see that the application was stopped:
```erlang-repl
=INFO REPORT==== 11-Feb-2013::12:46:40 ===
    application: overlord
    exited: stopped
    type: permanent

(eviltwin@10.1.1.2)1> 
```

# Credits

I did a lot of the code while reading "Learn you Some Erlang".
This is a very cool book, check it out.

Specifically, the following chapters were very helpful:
 * http://learnyousomeerlang.com/distribunomicon
 * http://learnyousomeerlang.com/building-applications-with-otp
 * http://learnyousomeerlang.com/distributed-otp-applications
