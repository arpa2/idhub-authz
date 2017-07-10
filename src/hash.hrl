
-type hashalgo()    :: {crc32mod,integer()}
		     | {crc32mask,integer()}
		     | literal
		     | atom.

-type hashoutcome() :: integer() | binary().

-type hashvalue()   :: {hashalgo(),hashoutcome()}.

