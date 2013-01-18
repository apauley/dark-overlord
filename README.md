How can we demo distributed Erlang if not by having a dark overlord control an army of minions...

## The Dark Overlord: Starting the master node

$ erl -sname hypnosponge@overlord -setcookie TheHypnoSpongeKnowsBest

## The Minions: Connect to the dark overlord

erl -sname minion -connect_all false -setcookie TheHypnoSpongeKnowsBest -s net_adm ping_list hypnosponge@overlord

(minion@iris)1> nodes().
[hypnosponge@overlord]
(minion@iris)2> net_adm:ping('hypnosponge@overlord').
pong
(minion@iris)3> 
