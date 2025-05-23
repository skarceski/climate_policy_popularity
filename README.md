# climate_policy_popularity

This repository contains the replication files for the paper, "Comparing and explaining the popularity of 99 climate policies." 

The `data` folder contains three data files. 
- `stage_1_cb.csv` contains the full text of each policy (`policy`) for matching with the policy id (`item`). It also contains information on the policy type (`type`).
- `stage_1_data.csv` contains all the survey data in a wide format.
- `stage_1_policies_long.csv` contains the responses to the policy questions in a long format. There are three calculations for policy preferences:
  - `score` is the response coded on a five-point numeric scale (from -2/strongly oppose to +2/strongly support).
  - `np` is the response coded on a three-point scale, where +1 indicates support (strongly or somewhat) and -1 indicates opposition (strongly or somewhat). "Don't know enough to say" responses are coded as zeros.
  - `sup` indicates whether the respondent supports the policy. It takes a value of 1 when the respondent supports the policy, a zero when the respondent opposes ("Don't know enough to say" are coded as missing).

The `syntax` folder contains one file `replication_code.R` that includes all the R code necessary to replicate the figures and table from the paper. 
