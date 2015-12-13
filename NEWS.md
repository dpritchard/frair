# FRAIR v0.4.1

## Major changes
- Moved to the 'lamW' package for lambertW estimation. 
	- This should improve performance markedly.  
	- This drops the dependancy on 'emdbook', which in turn drops the dependancy on 'rgl' which requires X11 on MacOS.  

- Changes to the Beddington-DeAngelis Type-II Model 
	- What had been advertised as the Beddington-DeAngelis Type-II in FRAIR (`bdII`, `bdII_fit`, `bdII_nll`, etc) in versions prior to 0.4.1 was not correctly specified. 
	- The code used for these functions was taken directly from Bolker 2008 (Ecological Models and Data in R). This model includes a fixed term `P` which does little more than "partition" the fitted coefficients between multiple predators.
	- The original functions were fully deprecated. They now throw an error. 
	- This code is now available as `emdII`. Hopefully this name better reflects it's heritage.  


	
	
## Minor changes
- Most "external" functions are called through explicit reference to the package (via '::'). Hopefully this makes it easier for someone else to take over in the future.  