# frair 0.5.202

- Expand the scope of `\donttest{}`. This is likely due to some parallelism in external packages that is currently difficult (for FRAIR) to control.

# frair 0.5.201

- Wrap some examples in `\donttest{}` to speed up automated checks on CRAN. All examples are still there, but some do not run by default with `R CMD check` or `example()` (when `interactive()` returns FALSE)

# frair 0.5.200

- Update Rd cross-references to Rd package anchors, where relevant. 
- Reformat NEWS.md, removing "v." in front of the version number. Should parse now.
- Cleaned up deprecated functions in tests.
- Tidy up URLs in README.md

# frair 0.5.100

- Messages are now dispatched via `message()` not via `cat()`.  This is the right thing to do.  
- Speeling mistakes corrected, throughout (this will probably be an ongoing issue!).
- Update T in examples using Gammarus and Bythotrephes.
	- It is best practice to use either "day" or "hour" as the basis for "T". We have chosen "day".
- Update frair_fit help page
	- Provide an example on using AIC to compare (nested) models
	- Provide an example of how T modifies units on coefficients.  
	- Provide more information for users looking to circumvent FRAIR’s limitations.
- Update the rogers_II help page
	- Note that Royama (1971) described the Rogers equation first.

# frair 0.5

## Important user-facing changes
- Changes to the Beddington-DeAngelis Type-II model 
	- What had been advertised as the Beddington-DeAngelis Type-II in FRAIR (the `bdII*` family) in versions prior to 0.5 was not correctly specified. 
	- The code used for these functions was taken directly from Bolker 2008 (Ecological Models and Data in R). This model includes a fixed term `P` which only "partitions" the fitted coefficients between multiple predators.
	- The original functions are now fully deprecated and removed from FRAIR. Trying to use them will throw an error. 
	- The original code is now available as `emdII`. Hopefully this name better reflects its heritage. 
- Changes to the real77 and real77r Models
    - Prior to version 0.5 the `real77*` family was internally consistent, but not terribly well specified. It is probable that the old specification made curves difficult to fit and generated non-sensical 'q' parameters. 
    - The `real77*` family has now been fully deprecated and removed from FRAIR. Trying to use these functions will throw an error.  
    - More sensible "flexible exponent" model specifications (`flexp` and `flexpnr`) have been added as a replacement. 
- Changes to the hassIIIr model
    - Nothing substantive has changed internally, however this model is now called `hassIIInr`, which better reflects that it is a **N**on-**R**eplacement model.  
    - Attempting to use a `hassIIIr*` function directly will pass through to `hassIIInr`, with a warning.
    - That said, `hassIIIr` is no longer recognised by `frair_responses()`, so attempting to fit this model in the FRAIR framework will throw an error.  
    
## Minor user-facing changes
- New `Bythotrephes` Data
	- We have (re)introduced the `bythotrephes` dataset. It describes *Bythotrephes- spp. (water fleas) preying on prey items of different sizes.
- Minor changes to `print.frboot` removing an unnecessary extra line break. Yep. Big stuff! 
- When the `tozero` argument of `lines.frfit`, `lines.frboot` and `drawpoly.frboot` are TRUE, these functions try to plot to zero. If that results in an undefined value for the fitted response, then it draws to 1e-04 instead and a warning is thrown. Note this does not guarantee all bootstrapped fits will be defined at this value either. 

## Code and backend changes
- Moved to the 'lamW' package for lambertW estimation. 
	- This should improve performance.  
	- This drops the dependency on 'emdbook', which in turn drops the dependency on 'rgl' which requires X11 on MacOS.  
- Most "external" functions are called through explicit reference to the package (via '::'). Hopefully, this makes it easier for someone else to take over in the future.  
- All of the internal `get` calls are scoped to 'package:frair'. This means it's no longer possible (whether accidentally or deliberately) to 'overwrite' FRAIR functions with those in the global namespace. This "functionality"" might return in the future alongside some formal documentation for extending FRAIR. 
- Tests! Finally.  

## Temporary changes
- The call to `boot` **does not*- use `::`.  The reason is that in the (fairly likely) scenario that users go digging into the structure of the frboot objects and call `obj$fit` directly, the `boot::boot` in the call confuses `print.boot` and it reports some pretty crazy stuff! 