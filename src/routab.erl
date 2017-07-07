% router.erl -- Redistribute messages to suitable cluster node
%
% This module receive requests from Diameter connection handlers
% and forwards them to other nodes elsewhere.  This module localises
% knowledge about the distribution of domains over nodes.  It runs
% a separate process for each request, and dedicates that to the
% monitored distribution of requests to other nodes.  Eventually,
% the response is shipped back to the Diameter connection.
%
% When it starts a handler for a group of domains, this module
% boasts about it to all its peers in the cluster.  Router nodes
% accept additions to the routing table from peers, by adding
% their Pids to the list of target nodes for a given group of
% domains.  Upon detected failure of such a Pid, it is taken
% out of the local routing table; explicit withdrawals such
% as through 'DOWN' signals will achieve the same.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( router ).


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


