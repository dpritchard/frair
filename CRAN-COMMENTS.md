Kia ora (Hello) from New Zealand.  

## 0.5.200
This is a minor update, addressing Rd cross-references (following an email received on 2025-03-24). This and other minor changes are documented in NEWS.md (which should now parse!). 

## Test environments
- Local macOS (Sequoia, 15.3.2: R 4.4.3)
- Win-Builder (R 4.3.3, 4.4.3 and 4.5.0 RC)

## R CMD check results
There were no ERRORs or WARNINGs
Win-Builder flagged some NOTEs for long-running examples... Depending on the build, these can be (system + user) between 10 and 26 seconds. This is expected, but if it is a problem I can look to optimise or remove the long-running examples.

## Downstream dependencies
This package still has no downstream dependencies. I have never expected there would be, so that is good.