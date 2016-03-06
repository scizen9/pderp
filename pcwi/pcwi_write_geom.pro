; $Id: pcwi_write_geom.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights reserved.
;+
; NAME:
;	PCWI_WRITE_GEOM
;
; PURPOSE:
;	Writes out the PCWI_GEOM struct as an IDL save file
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_WRITE_GEOM,Ppar,Pgeom
;
; INPUTS:
;	Pgeom	- PCWI_GEOM struct
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	TEST	- only write out if pgeom.status=0 (good fit)
;
; OUTPUTS:
;	None.
;
; PROCEDURE:
;	Uses the tag pgeom.geomfile to write out the struct as an IDL
;	save file.  Checks if ppar.clobber is set and takes appropriate
;	action.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-11	Initial Revision
;	2016-MAR-03	Split from kderp to pderp
;-
;
pro pcwi_write_geom,ppar,pgeom, test=test
;
; startup
pre = 'PCWI_WRITE_GEOM'
q = ''
;
; check inputs
if n_params(0) lt 1 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Ppar, Pgeom'
	return
endif
;
; Check structs
if pcwi_verify_geom(pgeom,/init) ne 0 then return
if pcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check fit status
if keyword_set(test) and pgeom.status ne 0 then begin
	pcwi_print_info,ppar,pre,'Pgeom fit no good, nothing written.',/error
	return
endif
;
; write it out
; check if it exists already
if file_test(pgeom.geomfile) then begin
	;
	; clobber it, if requested
    	if ppar.clobber eq 1 then begin
		file_delete,pgeom.geomfile,verbose=ppar.verbose
		pcwi_print_info,ppar,pre,'deleted existing geom file', $
			pgeom.geomfile,format='(a,a)'
	endif else begin
		pcwi_print_info,ppar,pre, $
			'existing geom file undisturbed', $
			pgeom.geomfile,format='(a,a)'
		return
	endelse
endif
;
; write it out if we get here
save,pgeom,filename=pgeom.geomfile
pcwi_print_info,ppar,pre,'wrote geom file',pgeom.geomfile,format='(a,a)'
;
return
end
