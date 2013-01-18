How can we demo distributed Erlang if not by having a dark overlord control an army of minions...

## The Dark Overlord: Starting the hypno-sponge

erl -sname hypnosponge@overlord -setcookie TheHypnoSpongeKnowsBest

## The Minions: Connect to the dark overlord

erl -sname minion -setcookie TheHypnoSpongeKnowsBest -s net_adm ping_list hypnosponge@overlord

(minion@watt)1> nodes().
[hypnosponge@overlord]
(minion@watt)2> net_adm:ping('hypnosponge@overlord').
pong
(minion@watt)3> 
