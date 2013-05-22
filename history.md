# Development History

This file documents additional information not yet ported to, nor ever very likely to be ported to, the main R documentation.   

## v. 0.1
Initial commit and port from dgmisc

## v. 0.2 
Addition of plotting code and other useful methods

### v. 0.2.5
Fixed bug in error catching (we now follow the 10%-50% rule).    
Added 'strata' for specification of stratified bootstrapped.   

### v. 0.2.6
Fixed bug in colour handelling for bootstrapped lines.
- Colour specification for lines.frboot(x, all=T) now requires bootcol and bootalpha
Removed upper limits on 'a' and 'h'
