% txn.erl -- Transaction Monitor for cluster-coordinated majority
%
% This module manages transactions based on majorities.  While
% opening a transaction, it will ask enough nodes to initially
% be on our side, and refer any others that ask for the same
% to our process.
%
% When other contenders contact us to open a transaction, we will
% compare our state to theirs, and come to the same conclusion
% as they would.  We may choose to take their side, and inform
% our initial collaborators of that fact.  If we win, the other
% side is expected to do that.
%
% Once we have a majority we can enter the second phase, in which
% we force collaboration from all nodes in the cluster.  Majority
% means that out of N nodes, at least (N div 2)+1 are on our
% side (in which, incidentally, we are the +1 node so we need to
% convince (N div 2) others to establish a majority.
%
% When nodes are down, they will not be able to cooperate with
% us.  That's quite alright; the majority can still rule and
% avoid a split-brain situation for the cluster.  The others
% will need to get on board sometime later.  It may not be
% possible to establish a majority in all cases; for instance,
% with only 2 nodes, or when the network splits in more than
% 2 partitions.
%
% The transaction monitor is aware of current and upcoming
% transactions, as well as ones that were not fully processed
% by all nodes.  Any transactions that were missed are a sign
% that a majority continued to be updated, and that detached
% nodes need to resynchronise.
%
%TODO% How to get back up without loosing transaction data
%
% From: Rick van Rein <rick@openfortress.nl>


%
% NOT CURRENTLY USED
%
% This module is not currently added to idhub-authz.
% The initial design assumes that all the frag processes
% will be locally updated to the LDAP configuration,
% thereby avoiding the need for distributed transactions.
%
% What transactionality remains, is easily handled by the
% process distributing changes to local frag processes.
% It is that process that will at some point be the local
% client to this module, when we shift to one feeding
% point and distribution of all the changes.  We may not
% ever decide to go there, however, as the locally fed
% model supports change even in split-brain situations.
%


