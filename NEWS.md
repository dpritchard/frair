# FRAIR v0.4.1

## Major changes
- Moved to the 'lamW' package for lambertW estimation. 
	- This should improve performance.  
	- This drops the dependancy on 'emdbook', which in turn drops the dependancy on 'rgl' which requires X11 on MacOS.  

- Changes to the Beddington-DeAngelis Type-II Model 
	- What had been advertised as the Beddington-DeAngelis Type-II in FRAIR (`bdII`, `bdII_fit`, `bdII_nll`, etc) in versions prior to 0.4.1 was not correctly specified. 
	- The code used for these functions was taken directly from Bolker 2008 (Ecological Models and Data in R). This model includes a fixed term `P` which only "partitions" the fitted coefficients between multiple predators.
	- The original functions are now fully deprecated and removed from FRAIR. Trying to use them will throw an error. 
	- The orginal code is now available as `emdII`. Hopefully this name better reflects it's heritage. 

- Changes to the real77 and real77r Models
    - Prior to version 0.4.1 the real77* family were internally consistent, but not terribly well specified. It is probable that the old specification made curves very hard to fit and generated non-sensical 'q' parameters. 
    - The real77* family have now been fully deprecated and removed from FRAIR. Trying to use them will throw an error.  
    - More sensible "flexible exponent" model specifications (`flexp` and `flexpnr`) have been added as a replacement. 
	
## Minor changes
- Most "external" functions are called through explicit reference to the package (via '::'). Hopefully this makes it easier for someone else to take over in the future.  