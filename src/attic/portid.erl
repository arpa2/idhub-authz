% portid.erl -- Client implementation of RFC 1413 "auth" or "ident" protocol
%
% Ident is a simple way to ask for a client's identity, which is totally
% insecure _except_ on a protected / in-house network.  Queries are sent to
% the client's IP address (which is constrained to a secure subset) and
% mention the TCP ports on both ends of a connection for which we want to
% learn the client identity:
%
%	serverport,clientport\r\n
%
% The response is one of:
%
%	serverport,clientport:ERROR:Reason\r\n
%	serverport,clientport:USERID:ARPA2:john@example.com\r\n
%	serverport,clientport:USERID:ARPA2:example.com\r\n
%
% The ARPA2 word indicates our own flavour; it is a non-standard naming scope,
% which we use to provide the authenticated identity in the form of a DoNAI,
% so a user@domain.name or a domain.name.  Note that the latter form is a
% common habit for domain-centric servers who send on behalf of a domain user,
% such as SMTP, XMPP and similar servers.  The assumption made in the
% InternetWide Architecture is that a domain-authenticated server certificate
% may speak on behalf of domain users.  Naturally, end users are not expected
% to carry such certificates; if they have a certificate, it will be put in
% their personal name by adding a user identity, an email address or so on.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( portid ).


-export([
	init/1,
	loop/1
]).


%%
%% Registered rfc1413_auth service.  This is an internal service that accepts
%% {query,HostName,PortPairString} queries.  It maintains a pool of
%% connections for accepted HostName values and responds either with an error,
%% {error,Reason}, or with success, {arpa2>,<<"john@example.com">>}.
%%
%%TODO%% We don't need such unique tags.  Still a good idea to avoid accidents?
%%



init( [HostName] ) ->
		ReERROR = re:compile( <<"^[ \t]*([0-9]+)[ \t]*,[ \t]*([0-9]+)[ \t]*:[ \t]*ERROR[ \t]*:(.*)$">> ),
		ReARPA2 = re:compile( <<"^[ \t]*([0-9]+)[ \t]*,[ \t]*([0-9]+)[ \t]*:[ \t]*USERID[ \t]*:[ \t]*ARPA2[ \t]*:(.*)$">> ),
		Re = #{
			error => ReERROR,
			arpa2 => ReARPA2
		},
		loop( {HostName,closed,<<"">>,#{},Re} ).

loop( {HostName,Cnx,Buf,Cli,Re} ) ->
		receive

		% Process a query from a Pid, after SrvPort,CliPort
		% aimed at our HostName
		%
		{query,Pid,HostName,SrvPort,CliPort} ->
			case Cnx of
			closed ->
				NewCnx = gen_tcp:connect( HostName,113);
			_ ->
				NewCnx = Cnx
			end,
			Request = << SrvPort, ",", CliPort, "\r\n" >>,
			%TODO% What happens if the following fails late?
			gen_tcp:send( NewCnx,Request ),
			NewCli = maps:merge( Cli, #{{SrvPort,CliPort}=>Pid} ),
			loop( {HostName,Cnx,Buf,NewCli,Re} );

		% Process response Data after appending it to Buf;
		% remove the processed part and place the remainder in new Buf
		{tcp,Cnx,Data} ->
			{NewBuf,NewCli} = process_lines( <<Buf,Data>>,Cli,Re ),
			loop( {HostName,Cnx,NewBuf,NewCli,Re} );

		% Process closing of the TCP connection but prepare to loop
		% Ignore any half response still in Buf but do hint clients
		{tcp_closed,Cnx} ->
			closedown( maps:values( Cli )),
			loop( {HostName,closed,<<"">>,#{},Re} )

		% No more acceptable forms of message
		end.

closedown( [] ) ->
		ok;
closedown( [Pid|More] ) ->
		Pid ! {error,disconnect},
		closedown( More ).

process_lines( Buf,Cli,Re ) ->
		case binary:split( Buf, <<"\r\n">> ) of
		[Line,More] ->
			case re:run( Line,maps:get( Re,arpa2 ) ) of
			{SP,CP,UID} ->
				NewCli = reply( Cli,SP,CP,{arpa2,UID} );
			_ ->
				% parse a well-formed error response
				case re:run( maps:get( Re,error ) ) of
				{SP,CP,ERR} ->
					NewCli = reply( Cli,SP,CP,{error,ERR} )
				%BREAKDOWN% _ ->
				%BREAKDOWN% 	% quietly ignore parsing error
				%BREAKDOWN% 	NewCli = Cli
				end
			end,
			process_lines( More,NewCli,Re );
		_ ->
			{Buf,Cli}
		end.

reply( Cli,SrvPort,CliPort,Msg ) ->
		case maps:take( Cli,{SrvPort,CliPort} ) of
		{Pid,NewCli} ->
			Pid ! Msg,
			NewCli
		%BREAKDOWN% error ->
		%BREAKDOWN% 	% quietly ignore unsollicited responses
		%BREAKDOWN% 	Cli
		end.
