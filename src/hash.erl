% hash.erl -- Local knowledge about HashAlgo, HashValue and so on
%
% From: Rick van Rein <rick@openfortress.nl>


-module( hash ).


-include( "donai.hrl" ).
-include( "hash.hrl" ).


-export([
	compute/2,
	instances/1
]).


% Compute the hash of a domain name, given a HashAlgo.
%
-spec compute( hashalgo(),dom() ) -> hashvalue().
%
compute( HashAlgo={crc32mod,N}, Domain ) ->
		{HashAlgo,erlang:crc32( Domain ) rem N};
compute( HashAlgo={c3c32mask,M}, Domain ) ->
		{HashAlgo,erlang:crc32( Domain ) band M};
compute( HashAlgo=literal, Domain ) ->
		{HashAlgo,Domain};
compute( HashAlgo=atom, Domain ) ->
		{HashAlgo,erlang:binary_to_atom( Domain, utf8 )};
compute( HashAlgo,_ ) ->
		erlang:error( {unsupported,HashAlgo} ).


% Provide a complete HashValue list for the HashAlgo, if possible.
%
% For the literal and atom HashAlgo we cannot define instance
% information because these can never strive for completeness
% in coverage anyway.
%
-spec instances( hashalgo() ) -> [hashvalue()].
%
instances( literal ) ->
		undefined;
instances( atom ) ->
		undefined;
instances( HashAlgo={crc32mod,N} ) ->
		lists:zip(
			lists:duplicate( N,HashAlgo ),
			lists:seq( 0,N-1 ) );
instances( HashAlgo={crc32mask,_M} ) ->
		instances_masked( HashAlgo,0 );
instances( HashAlgo ) ->
		erlang:error( {unsupported,HashAlgo} ).

% Iterating instances of masked hashes is a bit more difficult.
% When we "wrap around" due to a '0' mask bit we should bsl until
% we are once again in sight due to a '1' mask bit.  This applies
% only to the part concealed by the '0' mask bit, and we will
% have to do this repeatedly.  We already warned that this would
% not be the best possible algorithm to use...
%
-spec instances_masked( {crc32mask,integer()},integer() ) -> [hashvalue()].
%
instances_masked( {crc32mask,M},I )
		when I > M ->
			[];
instances_masked( HashAlgo={crc32mask,M},I )
		when (I band M) /= I ->
			Visible = I band M,
			Concealed = I bxor Visible,
			NextAttempt = Visible + (Concealed bsl 1),
			instances_masked( HashAlgo,NextAttempt );
instances_masked( HashAlgo={crc32mask,_M},I ) ->
		[ {HashAlgo,I} | instances_masked( HashAlgo,I+1 ) ].


