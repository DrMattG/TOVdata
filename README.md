# Analysis of TOV bird data

- [x] Set up work-flow 
- [x] Test work-flow with Trønderlag subset of TOVE data
- [ ] Increase samples and look at convergence
- [ ] Discuss outputs/graphics with @Diego
- [ ] Run work-flow on full dataset

_________________

**Using targets**

To use {targets} in this repo you can look at the workflow and then extract the result of a target using the following code:

library(targets)

tar_visnetwork()# This will show the workflow

tar_read("Name of Target")
