; $Id: pcwi_get_digits.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology.  All rights reserved.
;+
; NAME: PCWI_GET_DIGITS
;
; PURPOSE:
;	Return the number of digits in the image number portion of
;	an image fits filename.
;
; CALLING SEQUENCE:
;	Return = PCWI_GET_DIGITS(ObsFname)
;
; INPUTS:
;	ObsFname	- Observation fits filename
;
; OUTPUTS:
;	None.
;
; RETURNS:
;	Number of digits in the image number part of the filename
;
; EXAMPLE:
;	IDL> print,kwi_get_digits('image1234.fits')
;	       4
;	IDL>
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-06-03	Initial version
;	2014-06-06	Fixed to handle leading zeros
;	2016-MAR-03	Split from kderp to pderp
;-
;
function pcwi_get_digits,obsfname
	fdecomp,obsfname,disk,dir,root,ext
	return,strlen(stregex(root,'[0-9]+',/extract))
end
