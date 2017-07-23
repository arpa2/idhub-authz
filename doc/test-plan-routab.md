Routab Test Plan
================

Protocol tests for the `routab.erl` and associated `heir.erl` functionality

### \#1.1 Simple: 1 node, 1 routab, 1/1 frags

-   One node, running one routab process

-   Bootstrap simplicity: start directly, without a heir process

-   Start it, check `routing_table` contents

-   Start a fragment, feed it somehow [initially, static data]

-   Bootstrap simplicity: start directly, without a heir process

-   Check `routing_table` contents

-   **TODO:** Map to `gen_server` and `supervisor` model

-   **TODO:** Feed data into `frag` processes

### \#1.2 Compiling: 1 node, 1 routab, 2/2 frags

-   Same as \#1.1, just need 2 fragments, and check intermediate results

### \#1.3 Timing out: 1 node, 1 routab, 2/3 frags

-   Same as \#1.2, but expect 3 fragments and be fed with 2

-   Ensure that it continues after 5 minutes of timeout

-   We should trim down the `init_timeout` option for routab to (say) 5s

### \#2.1 Simple: 1 node, 1 routab, 1/1 frags

-   One node, one supervisor for routab + heir

-   Run \#1.1

-   Kill routab, or one of the frags (various tests)

-   Wait for new routing table or frag

-   Check contents to have survived

### \#2.2 Compiling: 1 node, 1 routab, 2/2 frags

-   One node, one supervisor for routab + heir

-   Run \#1.2

-   Kill routab, or one of the frags (various tests)

-   Wait for new routing table or frag

-   Check contents to have survived

### \#2.3 Timing out: 1 node, 1 routab, 2/3 frags

-   One node, one supervisor for routab + heir

-   Run \#1.3

-   Kill routab, or one of the frags (various tests)

-   Wait for new routing table or frag

-   Check contents to have survived

-   We should trim down the `init_timeout` option for routab to (say) 5s

### \#3.1 Networked: 2 nodes, 2 routabs, 1/2 frags each

-   One node, create routab supervisors with different table names

-   Run \#2.2

### \#3.2 TripleNet: 3 nodes, 3 routabs, 1/3 frags each

-   Like 3.1 but 3 tables and 3 frags, 1 each per table

### \#3.3 Redundant: 3 nodes, 3 routabs, 2/3 frags each

-   Like 3.2 but 3 tables and 3 frags, 2 each per table

### \#3.4 Networked timeout: 2 nodes, 2 routabs, 1/3 frags each

-   One node, create routab supervisors with different table names

-   Run \#2.3

 
