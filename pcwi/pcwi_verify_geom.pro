; $Id: pcwi_verify_geom.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_VERIFY_GEOM
;
; PURPOSE:
;	This function verifies the input PCWI_GEOM struct.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_VERIFY_GEOM(Pgeom)
;
; INPUTS:
;	Pgeom	- PCWI_GEOM struct
;
; RETURNS:
;	The status of the input PCWI_GEOM struct:
;	0	- verified without problems
;	1	- a malformed or uninitialized PCWI_GEOM struct was passed
;
; KEYWORDS:
;	INITIALIZED - set to check if PCWI_GEOM struct is initialized
;	SILENT	- set to silence output
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-15	Initial version
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_verify_geom,pgeom,initialized=initialized,silent=silent
	;
	; setup
	pre = 'PCWI_VERIFY_GEOM'
	;
	; check input
	stat = 0
	sz = size(pgeom)
	if sz[0] ne 1 or sz[1] lt 1 or sz[2] ne 8 then begin
		if not keyword_set(silent) then $
			print,pre+': Error - malformed PCWI_GEOM struct'
		stat = 1
	endif else begin
		if keyword_set(initialized) then begin
			if pgeom.initialized ne 1 then begin
				if not keyword_set(silent) then $
					print,pre+': Error - PCWI_GEOM struct not initialized, run PCWI_TRACE_CBARS and PCWI_EXTRACT_ARCS first'
				stat = 1
			endif
		endif
	endelse
	;
	return,stat
end
