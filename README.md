# EOP
Source code for building the EOP's libraries for data viz and web maintenance
## About
This repository hosts code for the EOP.Maintenance R package used by Miami University's Economic Opportunity Project. This github was started to enable easy code sharing, process improvement suggestions, and collaborative web development for the EOP's website (fsb.muohio.edu/eop/).
### Package Elements
As of September 25th, 2018, the R package  consists of the following functionalities:

1. SA_EOP() function, the base function for all of the work done in support of the EOP. This function reads in the CPS data files created by Dr. Even and seasonally adjusts all of the 1000+ economic time series making up the CPS data, before spitting the data back out in the format it originally comes to us in.

2. get_bilatTrade() function, a function which goes to the Census Bureau's website and pulls in the US bilateral trade balance with a user's choice of countries

### Next Steps
Planned next steps for EOP maintenance are functions to reproduce all of the graphs on the EOP's home page, so that the graphs can be updated in an automated fashion every month once new data becomes available.
