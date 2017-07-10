% http.erl -- An API for HTTP authorisation requests (like for Nginx)
%
% This listens to a TCP interface and does a very simple (yet secure)
% parse of what we would call authorisation requests over HTTP:
% http://nginx.org/en/docs/http/ngx_http_auth_request_module.html
% Note that a deliberate design approach is to keep things simple,
% under the assumption that a local, well-behaved client connects.
%
% It expects the following headers:
%  - X-Realm: example.com
%    The realm / domain name under which we are working
%  - X-Authn: john@example.com
%    Fill this with the DoNAI form of an authenticated (known-good) identity
%  - X-Ident: host 12345,45678
%    In lieu of X-Authn, query host:113 using RFC 1413 to fetch john@example.com
%  - X-Access: ASDCWRPKOVwgb
%    Fill this with access letters from http://idp.arpa2.net/radius.html
%  - X-Resource: 191c39e4-ea1b-4d57-9919-68c98c921175
%    One or two UUIDs (space-separated if two) indicating a resource [instance]
%  - X-Comm: mary
%    The user[+label] target of an attempted communication.
%  - X-Impose: sales@example.com
%    Attempt to change our identity to the one given.
%  - X-TTL: 300 3600
%    Cache the authorisation result for 300 seconds (assuming the HTTP client
%    will also be kept open for that long, otherwise that is useless).
%    This renews cache entries; an optional second value, following after a
%    space, can give an absolute timeout to enforce on new cache entries.
%
% We may need an extra form for digging up the identities that we can map to.
%
% The response is very short, as only its response code matters:
%  - HTTP/1.1 200 OK
%    To indicate that access is granted
%  - HTTP/1.1 403 Forbidden
%    To indicate that access is not granted
%
% We do provide a few header with extra information:
%  - X-Access: ASDCWRPKOVwgb
%    These are the access rights granted to this connection.
%  - X-Authz: john@example.com
%    This is the user who was authorised because he matched the ACLs.
%    Use this for inclusion in Proxied Authorization, like RFC 4370
%
% Repeated inquiries will rely on buffering keyed by X-Authn, and kinds
% of authorisations stored for it.  The X-Access is not a key and may vary
% between requests, because authorisation services like Nginx' simply look for
% any previously granted permissions to see if all the requested bits were
% indeed granted.  The reachable idmap will be stored as part of the buffer.
%
% Note that the X-Ident alternative to properly supplied X-Authn limits this
% HTTP interface.  The assumption of RFC 1413 is that requests are sent from
% the same IP address as that used by the original query; this is achievable
% when a TLS tunnel always forwards over IPv6 say, but *only* when the query
% made here arrives at the local authorisation node.  In other words, there
% must be no mirrorred authorisation service configured in the web server.
% We can only conclude that X-Ident is not properly located in this module,
% it is merely a convenience for simple setups and we therefore deprecate it
% now, on its day of introduction: don't count on it in your live systems!
%
%TODO% Part of this module is generic; it is probably useful to create a
% keyed cache of client inquiries.  Caching of the idmap outcome seems useful,
% as it saves us some ardeous work, but caching the xsmap is far less likely
% to hit, and it saves much less work.
% 
% From: Rick van Rein <rick@openfortress.nl>

%TODO% The Db would normally be available inside of the domain processes

-module( http ).


-export([
	http_request/0,
	parse_request/1,
	handle_request/1, handle_request/0
]).


http_request() ->
	<<"GET / HTTP/1.0\r\n"
		"X-Realm: example.com\r\n"
		"X-Authn: john@example.com\r\n"
		"X-Access: WR\r\n"
		"X-Impose: john+sales@example.com\r\n"
		"X-TTL: 300 3600\r\n"
		"\r\n" >>.

parse_request( HR ) ->
	[_ReqLine|HeaderLines] = binary:split( HR,<<"\r\n">>,[global] ),
	% io:format( "HeaderLines: ~p~n",[HeaderLines] ),
	HeaderWords = [ binary:split( L,<<" ">> ) || L<-HeaderLines ],
	% io:format( "HeaderWords: ~p~n",[HeaderWords] ),
	ParseFun = fun( Elem,Accu ) -> parse_header( Elem,Accu ) end,
	Env = lists:foldr( ParseFun,#{},HeaderWords ),
	env_check( Env ).

xenv( Env={error,_},_,_) ->
		Env;
xenv( Env,K,V ) ->
		if maps:is_key( K,Env ) ->
			{error,ambiguous};
		true ->
			maps:put( K,V,Env )
		end.

% parse_header( _,{error,Reason} ) ->
% 		{error,Reason};
parse_header( [<<"X-Realm:">>,Realm],Env) ->
		xenv( Env,realm,Realm );
		% maps:put( realm,Realm,Env );
parse_header( [<<"X-Authn:">>,Authn],Env) ->
		xenv( Env,authn,Authn );
		% maps:put( Env,Authn,Env );
parse_header( [<<"X-Access:">>,Access],Env) ->
		xenv( Env,access,Access );
		% maps:put( access,Access,Env );
parse_header( [<<"X-Impose:">>,Impose],Env) ->
		xenv( Env,impose,Impose );
		% maps:put( impose,Impose,Env );
parse_header( [<<"X-Comm:">>,Comm],Env) ->
		xenv( Env,comm,Comm );
		% maps:put( comm,Comm,Env );
parse_header( [<<"X-Resource:">>|Resource],Env) ->
		xenv( Env,resource,Resource );
		% maps:put( resource,Resource,Env );
parse_header( [<<"X-TTL:">>,TTL],Env) ->
		xenv( Env,ttl,TTL );
		% maps:put( ttl,TTL,Env);
parse_header( [<<"">>|_Rest],Env ) ->
		Env;
parse_header( Hdr,_ ) ->
		{error,{badheader,Hdr}}.

%%%TODO%%% Overtaken by matching Env with all the required arguments
env_check( Env={error,_} ) -> Env;
env_check( Env ) ->
		Must = [ maps:is_key( K,Env ) || K <- [authn,access,realm] ],
		May  = [ maps:is_key( K,Env ) || K <- [impose,comm,resource] ],
		And = fun(X,Y) -> X and Y end,
		Ctr = fun(X,Y) -> case X of true -> Y+1; true -> Y end,
		case lists:foldl( And,true, Must ) and
		     lists:foldl( Ctr,0,    May  ) == 1 of
		true  -> Env;
		false -> {error,incomplete}
		end.

handle_request() ->
		handle_request( http_request() ).

handle_request( HTTPREQ ) ->
		Env = parse_request( HTTPREQ ),
		do_request( Env ).

do_request( #{authn:=Authn,access:=_Access,realm:=Realm,impose:=Target} ) ->
		Authn2 = donai:parse_fqn2adr( Authn, Realm ),
		Authz2 = donai:parse_fqn2adr( Target,Realm ),
		if (Authn2 == syntax_error) or (Authz2 == syntax_error) ->
			{error,syntax};
		true ->
			io:format( "~p attempts to pose as ~p under ~p~n",[Authn2,Authz2,Realm] ),
			io:format( "~p could impose as ~p~n",[Authn2,
				idmap:impose(
					idmap_tests:testdb(),
					Realm,
					Authn2 ) ] ),
			authz:impose_as(
				idmap_tests:testdb(),
				Realm,
				Authn2,
				Authz2 )
			%TODO% Hold against _Access
		end;
do_request( #{authn:=Authn,access:=_Access,realm:=Realm,comm:=Target} ) ->
		Authn2 = donai:parse_fqn2adr( Authn, Realm ),
		Authz2 = donai:parse_fqn2adr( Target,Realm ),
		if (Authn2 == syntax_error) or (Authz2 == syntax_error) ->
			{error,syntax};
		true ->
			authz:communication_access(
				authz_tests:testdb(),
				Realm,
				Authn2,
				Authz2 )
			%TODO% Hold against _Access
		end;
do_request( #{authn:=Authn,access:=_Access,realm:=Realm,resource:=Target} ) ->
		Authn2 = donai:parse_fqn2adr( Authn, Realm ),
		if Authn2 == syntax_error ->
			{error,syntax};
		true ->
			authz:resource_access(
				authz_tests:testdb(),
				Realm,
				Authn2,
				Target )
			%TODO% Hold against _Access
		end;
do_request( Env={error,_} ) ->
		Env;
do_request( _ ) ->
		{error,badarg}.

