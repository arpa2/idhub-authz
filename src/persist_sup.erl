% persist_sup.erl -- Supervisor for a pair of data node and heir
%
% The data node that assigns a heir is a standard pattern in the
% idhub-authz application.  The two find each other through the
% routing_table, if it isn't the routab process itself.  They
% hint at each other about heritage, and during death of the
% data process the table(s) get converted to the heir, which will
% give it back when a newly started data manager asks for them.
%
% Data sets are named as either the atom routab, or a HashValue
% tuple {HashAlgo,Output}.  Both children of the pair are
% initialised with the data set name as parameter.  Within the
% pair, we have a data process and an heir, named by the atoms
% dataset and heir, respectively.  Neither of these names are
% registered, nor is a supervisor name.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( persist_sup ).
-behaviour( supervisor ).

-export([
	start_link/1,
	init/1,
	stop/0
]).


start_link( DatasetName ) ->
		supervisor:start_link( ?MODULE, [DatasetName] ).

init( DatasetName ) ->
		case DatasetName of
		routab ->
			DatasetFMA = {routab,start_link,[]};
		{_HashAlg,_Output} ->
			DatasetFMA = {frag,start_link,[DatasetName]}
		end,
		HeirFMA = {heir,start_link,[DatasetName]},
		{ ok, {
			{one_for_one,10,300}, [
				{ dataset,DatasetFMA },
				{ heir,HeirFMA }
			] }
		}.

stop() ->
		'TODO'.
		
