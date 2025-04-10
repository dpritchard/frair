Kia ora anō (Hello again) from New Zealand.

This is a resubmission of 0.05.203, with a version bump to 0.5.203

## 0.5.203
Finally I understand the pre-test NOTEs about 

## 0.5.202
Still tracking down NOTEs in the pre-tests that don't flag elsewhere. I have expanded the scope of the `\donttest{}` blocks, which I think (hope) might now be sufficient.

## 0.5.201
As noted previously:

> Win-Builder flagged some NOTEs for long-running examples... Depending on the build, these can be (system + user) between 10 and 26 seconds. This is expected, but if it is a problem I can look to optimise or remove the long-running examples.

This prevented automatic checks passing, so I have wrapped some longer-running examples in `\donttest{}` and skipping some tests using `skip_on_cran()`. 

## 0.5.200
This is a minor update, addressing Rd cross-references (following an email received on 2025-03-24). This and other minor changes are documented in NEWS.md (which should now parse!). 

## Test environments
- Local macOS (Sequoia, 15.3.2: R 4.4.3)
- Win-Builder (R 4.3.3, 4.4.3 and 4.5.0 RC)

## R CMD check results
There were no ERRORs or WARNINGs and (hopefully) no NOTEs for long running examples (or any other reason).

## Downstream dependencies
This package still has no downstream dependencies. I have never expected there would be, so that is good.