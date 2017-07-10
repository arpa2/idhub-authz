% frags_sup.erl -- The collection supervisor for fragment data sets
%
% The fragments collection is a supervisor that holds any number of
% the same-shaped information, namely a data set that is identified
% by a HashValue, so {HashAlgo,Output}.  Each of these contains a
% fragment of the domains that are supported in the entire cluster.
% Operators configure the fragments to be supported on each node.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( frags_sup ).
-behaviour( supervisor ).


-export([
	start_link/0,
	init/1,
	stop/0
]).


start_link() ->
		supervisor:start_link( ?MODULE,[] ).

init( [] ) ->
		{ ok, {
			{simple_one_for_one,10,300}, [] }
		}.

stop() ->
		'TODO'.


