# Stage 3G Wild Cluster Singleton Fix V2

Run the workflow named:

Paper1 Stage3G Wild Cluster Singleton Fix V2

The key proof that this version ran is the file:

06a_wildcluster_sample_audit.csv

This version:
- rebuilds exact complete-case samples;
- iteratively removes borrower-country and year singleton FE cells;
- disables additional fixest singleton removal;
- tries boottest with cluster-name input first;
- retries with an explicit cluster vector if needed;
- saves per-hypothesis boottest print and summary output;
- runs 9,999 Rademacher bootstrap draws;
- updates the final manuscript-style results table.

No new outcomes or hypotheses are introduced.
