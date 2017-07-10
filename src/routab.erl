% routab.erl -- Management of routing tables, spreading the word
%
% This module manages the routing table for a cluster of
% IdentityHub Authz machines.  Usually, each machine will handle
% only a portion of the work load, and knowing what domains are
% hosted where is a vital function.  This module does not
% perform any routing, but it communicates with its peers to
% maintain a routing table.
%
% Domains can be mapped in a number of ways.  First, the names
% are hashed, then their modulo-after-division is determined
% and then, based on the outcome, a set of targets is found.
% This routing information is stored in an ETS table, and
% shared with the processes that do the actual routing.
%
% Hashing algorithms include:
%  - crc32 over the domain name (default)
%  - atom for the complete domain name (good for small scale)
%  - integer holding the complete domain name (better scaling)
%
% A routing table is always specific for one hashing algorithm,
% and maintains a mapping of the outcome modulo-something.
% That something is basically configured, though there may be
% multiple settings active at the same time, in which case
% the routing table maintains their smallest common multiplier.
%
% Domain-name knowledge is hosted elsewhere; these may in
% use the smaller modulo that they were configured for, and
% when the routab uses a higher modulo it will have multiple
% references to peers.  This incongruence can be useful when
% changing from a modulo-3 network to a modulo-5 network, to
% given an example, at a time that machines are added to the
% cluster.  The routab then organises the change to new
% tables, and ends up destroying the old tables once their
% contents have been reassigned.
%
% The routab is acutely aware of the state of domain handlers,
% and grants them the time to (re)load domain data before it
% announces their coverage to peers and to local using
% processes.  When a domain handler dies, the routab will
% remove the entries both locally and with peers.  And
% when a peer node is unavailable, the routab will be cleansed
% from the entries leading there and a hope for future updates
% is awaited; the routab will also return that favour.
%
% The simplest possible routab, which is used when no
% configuration is available, is a (mod 1) over a crc32
% domain.  This covers all domains because the outcome is
% always 0.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( routab ).

-include( "hash.hrl" ).

-export([
	init/1,
	loop/1
]).


%TODO% Init function:
% 1. Find the list of target domains
% 2. Start a new node for each of them
% 3. Cause these nodes to be fed by their peers in the cluster
% 4. Once setup, announce nodes' Pids to routers in other peers
%
%TODO% Current implementation: Setup the one-and-only authz node


%TODO% Call function:
%  1. Map the target domain to a list of servicing nodes
%  2. If the local node is among them, put it in first position
%  3. Forward the call to each node, trying the next upon failure
%  4. When a node fails, remove its Pid from our local router table
%  5. When a node succeeds, report that back to our caller and end
%  6. Otherwise, when no options worked, terminaate our caller
%
%TODO% Current implementation: directly resolve matters here


%%
%% ROUTAB DATABASE MANAGEMENT
%%

-type localfragpidopt() :: pid() | none.
-type localheirpidopt() :: pid() | none.
-type remotefragpid()   :: pid().


%%
%% The following table, publicly named routing_table, is
%% managed by the publicly named routab process:
%% 
%% routing_table
%% -----------------------------+-------------------------------
%% HashValue                    | { LocalFragPidOpt,
%%                              |   [ RemoteFragPid ],
%%                              |   LocalHeirPidOpt }
%%
%% The LocalFragPidOpt represents a local frag process, if any
%% has registered.  The LocalHeirPidOpt represents the
%% corresponding heir process that will adopt the data from
%% the frag process if it breaks down, to expedite restarts.
%%
%% There should be precisely one frag and heir process for
%% each HashValue in routab, but only when served locally.
%% Since the processes register themselves, the routab is
%% initiated with them absent so queries initially direct
%% to other nodes, which is what [RemoteFragPid] supports.
%% These entries are taken from communication with other
%% routab processes in the cluster.
%%
%% The self-registration of frag and heir processes lets them
%% find each other through the routab, without having to
%% configure their relationship explicitly; the shared
%% HashValue alone suffices to bring them together.  The
%% routab process will take in new registrations and send
%% hints to the frag when a heir process registers, to
%% allow the frag to assign their heirloom to their heir.
%%
%% When an authorisation inquiry is started, it looks in
%% the public, named routing_table for the entries that is
%% maintained by the routab table.  Such lookups are made
%% without interacion with the routab table.
%%
%% Authorisation inquires are always first attempted on
%% the local node, but there is not always a suitable frag
%% on the local node, so this may not be possible.  The
%% fallback is then the list of remote nodes, each of which
%% will be tried in random order (thus spreading the load).
%% They will not be tried in parallel; that might advantage
%% an individual query, but at the expense of overall
%% efficiency.
%%



%%
%% ROUTAB MESSAGING
%%

% The following messages can be received by routab:
%
% {'DOWN',Ref,process,Pid,Reason}
%	A peer routab went offline; or
%	A frag process went offline
% {commfailure,HashValue,Pid}
%	An agent reports a non-responsive routab entry
% {commsuccess,HashValue,Node}
%	An agent reports a responsive routab entry
% {addentries_peer,[{HashValue,Pid}]}
% {delentries_peer,[{HashValue,Pid}]}
%	A peer routab reports a new frag process; and
%	A peer routab reports that a frag process is down
% {allentries_peer,Pid,[{HashValue,Pid}]}
%	A *complete* peer routab for the first Pid, but
%	otherwise the same as the differential update
%	in {addentries_peer,[{HashValue,Pid}]}
% {resync_peer,Pid}
%	A node needs to resync its routab with entries as
%	known to its peers; this may be because it is coming
%	online but would like to learn about peers' local
%	routing facilities first; the response is a single
%	{allentries_peer,[{HashValue,Pid}]} message with
%	all the local frag processes currently registered
% {add_local_fragment,HashValue,Pid}
% {del_local_fragment,HashValue,Pid}
%	A frag process reports that it is ready for duty
%	and that it is resigning from its duties
% {add_local_fragment_heir,HashValue,Pid}
% {del_local_fragment_heir,HashValue,Pid}
%	A heir process reports that it is ready for duty
%	and that it is resigning from its duties
% routab_heir
%	The routing_table heir reports for duty; it has
%	registered as process routab_heir at this point.
%
% The following messages may be sent by routab:
%
% getheir_frag
%	Sent to a frag to notify them of a hitherto unknown heir;
%	sent when a heir registers, or when a frag registers
%	while a heir is already registered.  Heirs do not get such
%	messages, as they will simply process ETS-TRANSFER when
%	their frag dies.
% {addentries_peer,[{HashValue,Pid}]}
% {delentries_peer,[{HashValue,Pid}]}
%	Inform a peer routab that we have new frag processes;
%	and inform a peer routab that frag processes went down.
% {allentries_peer,Pid,[{HashValue,Pid}]}
%	Full sync of the local frag processes in our routab node
%	which is mentioned in the 2nd tuple element.  This is also
%	the response to a newly started routab's {resync_peer,Pid}
%	where the requesting routab on that node cannot come
%	online before all its peers have responded.
%

loop( State ) ->
		receive
		{'DOWN',Ref_down,process,Pid_down,Reason} ->
			%TODO% Not implemented yet
			ok;
		{commfailure,HashValue,Pid} ->
			commfailure( HashValue,Pid );
		{commsuccess,HashValue,Pid} ->
			commsuccess( HashValue,Pid );
		{addentries_peer,Entries} ->
			addentries_peer( Entries );
		{delentries_peer,Entries} ->
			delentries_peer( Entries );
		{allentries_peer,Pid,Entries} ->
			allentries_peer( Pid,Entries );
		{resync_peer,Pid} ->
			resync_peer( Pid );
		{add_local_fragment,HashValue,Pid} ->
			add_local_fragment( HashValue,Pid );
		{del_local_fragment,HashValue,Pid} ->
			del_local_fragment( HashValue,Pid );
		{add_local_fragment_heir,HashValue,Pid} ->
			add_local_fragment_heir( HashValue,Pid );
		{del_local_fragment_heir,HashValue,Pid} ->
			del_local_fragment_heir( HashValue,Pid );
		routab_heir ->
			routab_heir()
		end,
		loop( State ).


%%
%% ROUTAB HANDLING OF INDIVIDUAL EVENTS OR MESSAGES
%%


% A peer has requested the current routab entries, so it can
% bootstrap itself and start its local agents.  This iterates
% over the routing_table and delivers the {HashValue,Pid_frag}
% for each entry with a local frag process.  The results are
% packed in a list and sent to the peer as
% {addentries_peer,[{HashValue,Pid}]}
% which the peer uses to setup its table.
%
%TODO% Maybe we should remove the remote node's old entries?
%
%TODO% We can relax by requiring a table with 100% coverage.
%TODO% We can incorporate locally registered knowledge too.
%
-spec resync_peer( pid() ) -> ok.
%
resync_peer( Pid_peer ) ->
		%TODO% We may end up registering repeatedly
		erlang:monitor( process,Pid_peer ),
		LocalFrag = ets:select(
			routing_table, [ {
				{'$1',{'$2','$3','$4'}},
				[is_pid, '$2'],
				{'$1','$2'} } ] ),
		% Lossy; the peer node cares for restarts
		Pid_peer ! {allentries_peer,self(),LocalFrag},
		ok.


%NEEDED?% % The resync_local process is needed for building up the
%NEEDED?% % routab, notably after a crash without backup.
%NEEDED?% %
%NEEDED?% %TODO% What updates did we miss after a crash?  Meaning, we
%NEEDED?% %	also miss out on things after a crash with backup,
%NEEDED?% %	though we might start immediated with on the old info
%NEEDED?% %	and process allentries_peer as we go.
%NEEDED?% %
%NEEDED?% -spec resync_local( TODO ) -> TODO.
%NEEDED?% %
%NEEDED?% resync_local() ->
%NEEDED?% 		TODO.
		


% Send a Message to our peer routab processes, which are
% assumed to currently run on all peer nodes; of course,
% when a remote routab is down, it is bound to ask for
% bootstrapping information through {resync_peer,Pid}
% so we can safely ignore such conditions.  For this
% reason, we can rely on lossy message passing.
%
%TODO% We might prefer to have a list of initiated peers?
%
-spec notify_peers( tuple() ) -> ok.
-spec notify_peers( tuple(),[node()] ) -> ok.
%
notify_peers( Message ) ->
		notify_peers( Message,erlang:nodes() ).
%
notify_peers( Message,PeerNodes ) ->
		[
			{PeerNode,routab} ! Message
		||
			PeerNode <- PeerNodes
		],
		ok.



% A frag has started and is announcing service to be up.
% We add it to the routing_table, and in case we already have
% a heir we will notify the new frag about its existence.
%
% The routab process monitors the new frag, so it will
% know when to remove its entry from routab.
%
%TODO% When an old frag exists, should we stop it?  It
% should not be alive anymore!
%
%TODO% Could a heir inherit from multiple sources?  Not
% usually, since the heir would have harvested what the
% frag used to hold.
%
-spec add_local_fragment( hashvalue(),pid() ) -> ok.
%
add_local_fragment( HashValue,Pid_frag ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			ets:insert( routing_table,{HashValue,Pid_frag,[],none} );
		[{HashValue,_OldFrag,RemoteFrag,FragHeirOpt}] ->
			% ignore _OldFrag even if it may be defined
			ets:insert( routing_table,{HashValue,Pid_frag,RemoteFrag,FragHeirOpt} ),
			if is_pid( FragHeirOpt ) ->
				Pid_frag ! update_heir;
			FragHeirOpt == none ->
				ok
			end
		end,
		notify_peers( {addentries_peer,[{HashValue,Pid_frag}]} ).

% A frag is shutting down and is renouncing its service.
% We delete it from the routing_table, ignoring any heir, as that
% will act on its own.
%
% The routab process monitors the new frag, so we have an alternative
% option for doing this, but %TODO% this does not appear necessary.
%
-spec del_local_fragment( hashvalue(),pid() ) -> ok.
%
del_local_fragment( HashValue,Pid_frag ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			% maybe awkward process crash timing got us here
			ok;
		[{HashValue,Pid_frag,RemoteFrag,FragHeirOpt}] ->
			% actually remove the local frag process
			ets:insert( routing_table,{HashValue,none,RemoteFrag,FragHeirOpt}),
			notify_peers( {delentries_peer,[{HashValue,Pid_frag}]} )
		end.

% A heir is announcing its service for a given HashValue.
% We enter it in the routing_table and, if we already have
% a frag process for the same HashValue, we will send a
% notification to it about the new heir.
%
-spec add_local_fragment_heir( hashvalue(),pid() ) -> ok.
%
add_local_fragment_heir( HashValue,Pid_heir ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			% insert heir before frag
			ets:insert( routing_table,{HashValue,none,[],Pid_heir}),
			ok;
		[{HashValue,Frag,RemoteFrag,_OldHeir}] ->
			% insert heir, possibly after frag
			ets:insert( routing_table,{HashValue,Frag,RemoteFrag,Pid_heir} ),
			if is_pid( Frag ) ->
				Frag ! update_heir,
				ok;
			Frag == none ->
				ok
			end
		end.

% A heir is renouncing its service for a given HashValue.
% We remove it from the routing_table and, if we already have
% a frag process for the same HashValue, we will send a
% notification to it about the lost heir.
%
-spec del_local_fragment_heir( hashvalue(),pid() ) -> ok.
%
del_local_fragment_heir( HashValue,Pid_heir ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			% maybe awkward process crash timing got us here
			ok;
		[{HashValue,Frag,RemoteFrag,Pid_heir}] ->
			% delete heir that does exist
			ets:insert( routing_table,{HashValue,Frag,RemoteFrag,none} ),
			if is_pid( Frag ) ->
				Frag ! update_heir,
				ok;
			Frag == none ->
				ok
			end
		end.


% The heir for the routing_table, so the backup for the routab process,
% reports for duty.  In response, routab will set it as its heir.
%
-spec routab_heir() -> ok.
%
routab_heir() ->
		case whereis( routab_heir ) of
		undefined ->
			ets:setopts( routing_table,{heir,none} );
		Pid_heir ->
			ets:setopts( routing_table,{heir,Pid_heir,routing_tabl} )
		end,
		ok.


% A peer reports a list of entries that it has added locally.
% This comes on top of the last {allentries,...} from the same node,
% so it is important to process any such message in order, so to
% always receive those messages in the same receive statement.
%
-spec addentries_peer( [{hashvalue(),pid()}] ) -> ok.
%
addentries_peer( [] ) ->
		ok;
addentries_peer( [{HashValue,Remote_pid}|More] ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			Frag = none,
			Heir = none,
			Remotes = [],
			IsMember = false;
		[{HashValue,Frag,Remotes,Heir}] ->
			IsMember = lists:member( Remote_pid,Remotes )
		end,
		if IsMember ->
			ok;
		true ->
			NewRemotes = [Remote_pid|Remotes],
			ets:insert( routing_table,{HashValue,Frag,NewRemotes,Heir} )
		end,
		addentries_peer( More ).


% A peer reports a list of entries that it has removed locally.
% This comes on top of the last {allentries,...} from the same node,
% so it is important to process any such message in order, so to
% always receive those messages in the same receive statement.
%
-spec delentries_peer( [{hashvalue(),pid()}] ) -> ok.
%
delentries_peer( [] ) ->
		ok;
delentries_peer( [{HashValue,Remote_pid}|More] ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			Frag = none,
			Heir = none,
			NewRemotes = [];
		[{HashValue,Frag,Remotes,Heir}] ->
			NewRemotes = lists:delete( Remote_pid,Remotes),
			ets:insert( routing_table,{HashValue,Frag,NewRemotes,Heir} )
		end,
		delentries_peer( More ).


% Setup a complete resync of the entries offered by a peer.
%
-spec allentries_peer( pid(), [{hashvalue(),pid()}] ) -> ok. 
%
allentries_peer( Pid_peer,Entries ) ->
		allentries_peer_remove( Pid_peer ),
		addentries_peer( Entries ).


% Remove all entries in the routing_table made by the peer
% routab.  This involves matching the nodes for stored pids.
%
%TODO% Is this desirable?  It is costly, how about commfailure?
%
allentries_peer_remove( Pid_peer ) ->
		%TODO% Not implemented yet.
		ok.


% An agent reports a problem communicating with a Pid.
%
%TODO% This is complicated by the intermediate authz handler process.
%
%TODO% Distinguish node failure from process failure.
%
%TODO% Distinguish process logic failure from process death.
%
-spec commfailure( hashvalue(),pid() ) -> ok.
%
commfailure( HashValue,Pid_failed ) ->
		%TODO% Not implemented yet
		ok.


% An agent reports a success communicating with a Pid.
%
%TODO% This is complicated by the intermediate authz handler process.
%
%TODO% This may be less useful since agents cannot distinguish nodes as failed ones.
%
-spec commsuccess( hashvalue(),pid() ) -> ok.
%
commsuccess( HashValue,Pid_succeeded ) ->
		%TODO% Not implemented yet
		ok.


%%
%% PROCESS CONTROL CODE -- INITIALISATION AND STARTING THE SERVICE LOOP
%%


init( State ) ->
		init_have_routing_table(),
		%TODO% Start the agent services, when they are stopped
		loop( State ).


% Ensure that the ETS table routing_table exists as a resource that
% we can write, and that can be publicly read.  If it already exists,
% continue using that (after taking it from its heir).
%
%TODO% pickup from heir if it sits there!
%TODO% parameterise table name so we can do more elaborate testing
%
-spec init_have_routing_table() -> ok.
%
init_have_routing_table() ->
		case {ets:info( routing_table,heir ),whereis( routab_heir )} of
		{none,Pid_heir} ->
			ets:new( routing_table, [
				set,
				protected,
				named_table,
				{write_concurrency,false},
				{ read_concurrency,true },
				compressed ] ),
			case Pid_heir of
			undefined ->
				%await routab_heir message
				ok;
			_ ->
				ets:setopts( routing_table,{heir,Pid_heir,routing_table} )
			end,
			%TODO% Set the heir for the table... using what Pid?!?
			% Send resync_peer, and wait for responses
			Peers = erlang:nodes(),
			notify_peers( {resync_peer,self()}, Peers ),
			[ init_allentries_peer( Peer ) || Peer <- Peers ],
			ok;
		{Pid_heir,Pid_heir} ->
			Pid_heir ! {hand_over,routing_table},
			% Send resync_peer, but don't wait for replies
			notify_peers( {resync_peer,self()} );
		{Other,Pid_heir} ->
			ets:setopts( routing_table,{heir,Pid_heir,routing_table} ),
			ok
		end.


% allentries_peer waits for the {allentries_peer,...} to resync the
% information from the given Pid_peer, and adds the entries when they
% arrive.  It discards any preceding content in {addentries_peer,...}
% and {delentries_peer,...} because these are soon to be overruled.
%
% This implements a forceful wait for the {allentries_peer,...} but
% it might return early when the peer dies before sending the desired
% message.  It is assumed that the peer will get back up and start
% delivering its local routing table.
%
-spec init_allentries_peer( pid()             ) -> ok.
-spec init_allentries_peer( pid(),reference() ) -> ok.
%
init_allentries_peer( Pid_peer ) ->
		Ref_peer = erlang:monitor( process,Pid_peer ),
		init_allentries_peer( Pid_peer,Ref_peer ).
%
init_allentries_peer( Pid_peer,Ref_peer ) ->
		receive
		{'DOWN',Ref_peer,process,Pid_Peer,Reason} ->
			% Peer died; panic; let go of it
			allentries_peer_remove( Pid_Peer ),
			ok;
		{delentries_peer,_Content} ->
			% Prior content is skipped
			init_allentries_peer( Pid_peer,Ref_peer );
		{addentries_peer,_Content} ->
			% Prior content is skipped
			init_allentries_peer( Pid_peer,Ref_peer );
		{allentries_peer,Pid_peer,Content} ->
			% Switch to the Content handler
			erlang:demonitor( Ref_peer ),
			allentries_peer( Pid_peer,Content )
		end.




