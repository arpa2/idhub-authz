%% Domain / Network Access Identifier
%%
%% These are the routines for parsing, comparing and otherwise processing
%% The DoNAI that we use as basic identifiers in the ARPA2 project, so
%% domain and user@domain; for internal interpretation conventions, we
%% distinguish the the form user+label@domain.
%%
%% In addition to these forms, we also accept patterns that we have called
%% DoNAI Selector form.  This form is used in Access Control Lists, namely
%% in black and white lists.
%%
%% We use binary() representations where we can; most addresses and
%% parts thereof are < 64 bytes, meaning they can be efficiently stored
%% by Erlang.  Since we may have a lot of data to store, this is important.
%% Longer identifiers are still no problem for Erlang in binary() form,
%% but are simply a little less efficient.
%%
%% http://donai.arpa2.net
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module( donai ).

-export([
	parse_fqn2dom/1, parse_fqn2dom/2,
	parse_fqn2adr/1, parse_fqn2adr/2,
	parse_fqnsel2domsel/1, parse_fqnsel2domsel/2,
	parse_fqnsel2adrsel/1, parse_fqnsel2adrsel/2,
	compare_uidsel2uid/2,
	compare_labsel2lab/2,
	compare_domsel2dom/2, compare_domsel2dom/3,
	compare_adrsel2adr/2, compare_adrsel2adr/3
]).


%
% DATA TYPE DECLARATIONS
%

-include( "donai.hrl" ).


%
% ADDRESS FORMAT PARSING (ALWAYS FROM BINARY)
%

%% Parse fqnsel() -> domsel(), or <<uidsel+labsel@domsel>> -> domsel()
%%
-spec parse_fqnsel2domsel( fqnsel() ) -> domsel() | syntax_error.
%%
parse_fqnsel2domsel( FQNSel ) ->
	All = byte_size(FQNSel),
	case binary:matches( FQNSel, << $@ >> ) of
	% Might use lists:last() for more flexibility
	[ { At, _One } ] ->
		case binary:part( FQNSel, At+1, All-At-1 ) of
		<< $. >> ->
			anything;
		<< $., More/binary >> ->
			{ subof, More };
		<< >> ->
			syntax_error;
		DomSel ->
			DomSel
		end;
	_ ->
		syntax_error
	end.

%% Parse fqnsel() -> domsel() but possibly return current
%%
-spec parse_fqnsel2domsel( fqnsel(), domcur() ) -> domsel() | syntax_error.
%%
parse_fqnsel2domsel( FQNSel, CurDom ) ->
	case parse_fqnsel2domsel( FQNSel ) of
	CurDom ->
		current;
	Other ->
		Other
	end.

%% Parse fqn() -> dom(), or <<uid+lab@dom>> -> dom()
%%
-spec parse_fqn2dom( fqn() ) -> dom() | syntax_error.
%%
parse_fqn2dom( FQN ) ->
	%OLD% All = byte_size(FQN),
	%OLD% % Might use lists:last() for more flexibility
	%OLD% [ { At, _One } ] = binary:matches( FQN, << $@ >> ),
	%OLD% binary:part( FQN, At+1, All-At-1 ).
	case parse_fqnsel2domsel( FQN ) of
	<< Dom/binary >> ->
		Dom;
	_ ->
		syntax_error
	end.

%% Parse fqn() -> dom() but possibly return current
%%
-spec parse_fqn2dom( fqn(), domcur() ) -> dom() | syntax_error.
%%
parse_fqn2dom( FQN, CurDom ) ->
	case parse_fqn2dom( FQN ) of
	CurDom ->
		current;
	Dom ->
		Dom
	end.

%% Parse fqnsel() -> adrsel(), or << uidsel+labsel@domsel >> -> { uidsel(), labsel(), domsel() }
%%
-spec parse_fqnsel2adrsel ( fqnsel() ) -> adrsel() | syntax_error.
%%
parse_fqnsel2adrsel( FQNSel ) ->
	case parse_fqnsel2domsel( FQNSel ) of
	syntax_error ->
		syntax_error;
	Dom ->
		[ { At, _One } ] = binary:matches( FQNSel, << $@ >> ),
		MatchPart = [ {scope, {0,At}} ],
		% Might use binary:matches() to constrain plusses
		case binary:match( FQNSel, << $+ >>, MatchPart ) of
		nomatch ->
			if At == 0 ->
				Uid = anything,
				Lab = optional;
			true -> 
				Uid = binary:part( FQNSel, 0, At ),
				Lab = absent
			end;
		{ Plus, _ } ->
			if Plus == 0 ->
				Uid = anything;
			true ->
				Uid = binary:part( FQNSel, 0, Plus )
			end,
			if At-Plus-1 == 0 ->
				Lab = anything;
			true ->
				Lab = binary:part( FQNSel, Plus + 1, At-Plus-1 )
			end
		end,
		{ Uid, Lab, Dom }
	end.

%% Parse fqnsel() -> adrsel() but possibly have dom==current
%%
-spec parse_fqnsel2adrsel ( fqnsel(), domcur() ) -> adrsel() | syntax_error.
%%
parse_fqnsel2adrsel( FQNSel, CurDom ) ->
	case parse_fqnsel2adrsel( FQNSel ) of
	{ Uid, Lab, CurDom } ->
		{ Uid, Lab, current };
	AdrSel ->
		AdrSel
	end.

%% Parse fqn() -> adr(), or << uid+lab@dom >> -> { uid(), lab(), dom() }
%%
-spec parse_fqn2adr( fqn() ) -> adr() | syntax_error.
%%
parse_fqn2adr( FQN ) ->
	%OLD% Dom = parse_fqn2dom( FQN ),
	%OLD% At = byte_size(FQN)-byte_size(Dom)-1,
	%OLD% MatchPart = [ {scope, {0,At}} ],
	%OLD% % Might use binary:matches() to constrain plusses
	%OLD% case binary:match( FQN, << $+ >>, MatchPart ) of
	%OLD% nomatch ->
	%OLD%	Uid = binary:part( FQN, 0, At ),
	%OLD%	Lab = undefined;
	%OLD% { Plus, _ } ->
	%OLD%	Uid = binary:part( FQN, 0, Plus ),
	%OLD%	Lab = binary:part( FQN, Plus + 1, At-Plus-1 )
	%OLD% end,
	%OLD% { Uid, Lab, Dom }.
	Adr = parse_fqnsel2adrsel( FQN ),
	case Adr of
	{ << _Uid/binary >>, Lab, << _Dom/binary >> } ->
		case Lab of
			<< _Lab/binary >> ->
				Adr;
			absent ->
				Adr;
			_ ->
				syntax_error
		end;
	_ ->
		syntax_error
	end.

% Parse fqn()->adr() but possibly have dom=current
%%
-spec parse_fqn2adr( fqn(), domcur() ) -> adr() | syntax_error.
%%
parse_fqn2adr( FQN, CurDom ) ->
	case parse_fqn2adr( FQN ) of
	{ Uid, Lab, CurDom } ->
		{ Uid, Lab, current };
	Adr ->
		Adr
	end.


%%
%% ADDRESS COMPARISON OPERATIONS
%%

%% Compare an Address Selector to an Address, without Current Domain
%%
-spec compare_adrsel2adr( adrsel(), adr() ) -> true | false.
%%
compare_adrsel2adr( {UidSel,LabSel,DomSel}, {Uid,Lab,Dom} ) ->
	compare_uidsel2uid (UidSel, Uid) andalso
	compare_labsel2lab (LabSel, Lab) andalso
	compare_domsel2dom (DomSel, Dom).

%% Compare an Address Selector to an Address, with Current Domain
%%
-spec compare_adrsel2adr( adrsel(), adr(), domcur() ) -> true | false.
%%
compare_adrsel2adr( {UidSel,LabSel,DomSel}, {Uid,Lab,Dom}, CurDom ) ->
	compare_uidsel2uid (UidSel, Uid) andalso
	compare_labsel2lab (LabSel, Lab) andalso
	compare_domsel2dom (DomSel, Dom, CurDom).

%% Compare a Uid Selector to a Uid
%%
-spec compare_uidsel2uid( uidsel(), uid() ) -> true | false.
%%
compare_uidsel2uid( anything, _ ) -> true;
compare_uidsel2uid( _Uid, _Uid ) -> true;
compare_uidsel2uid( _, _ ) -> false.

%% Compare a Label Selector to a Label
%%
-spec compare_labsel2lab( labsel(), lab() ) -> true | false.
%%
compare_labsel2lab( absent, absent ) -> true;
compare_labsel2lab( optional, _ ) -> true;
compare_labsel2lab( _, absent ) -> false;
compare_labsel2lab( anything, _ ) -> true;
compare_labsel2lab( _Lab, _Lab ) -> true;
compare_labsel2lab( _, _ ) -> false.

%% Compare a Domain Selector to a Domain, without a Current Domain
%%
-spec compare_domsel2dom( domsel(), dom() ) -> true | false.
%%
compare_domsel2dom( current, _ ) -> false;
compare_domsel2dom( anything, _ ) -> true;
compare_domsel2dom( _, current ) -> false;
compare_domsel2dom( { subof, DomEnd }, Dom ) ->
			Comparison = [ Dom, << $., DomEnd/binary >> ],
			SuffixLen = binary:longest_common_suffix ( Comparison ),
			SuffixLen == byte_size(DomEnd) + 1;
compare_domsel2dom( _Dom, _Dom ) -> true;
compare_domsel2dom( _, _ ) -> false.

%% Compare a Domain Selector to a Domain, given a Current Domain
%%
-spec compare_domsel2dom( domsel(), dom(), domcur() ) -> true | false.
%%
compare_domsel2dom( current, current, _ ) -> true;
compare_domsel2dom( current, _Dom, _Dom ) -> true;
compare_domsel2dom( current, _, _ ) -> false;
compare_domsel2dom( DomSel, current, Dom ) -> compare_domsel2dom( DomSel, Dom );
compare_domsel2dom( DomSel, Dom,     _   ) -> compare_domsel2dom( DomSel, Dom ).


