; $Id: pcwi_verify_ppar.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_VERIFY_PPAR
;
; PURPOSE:
;	This function verifies the input PCWI_PPAR struct.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_VERIFY_PPAR(Ppar)
;
; INPUTS:
;	Ppar	- array of struct PCWI_PPAR
;
; RETURNS:
;	The status of the input PCWI_PPAR struct:
;	0	- verified without problems
;	1	- a malformed or uninitialized PCWI_PPAR struct was passed
;
; KEYWORDS:
;	INITIALIZED - set to check if PCWI_PPAR struct is initialized
;	SILENT	- set to silence output
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-13	Initial version
;	2013-SEP-14	Added initialized keyword
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_verify_ppar,ppar,initialized=initialized,silent=silent
	;
	; setup
	pre = 'PCWI_VERIFY_PPAR'
	;
	; check input
	stat = 0
	sz = size(ppar)
	if sz[0] ne 1 or sz[1] lt 1 or sz[2] ne 8 then begin
		if not keyword_set(silent) then $
			print,pre+': Error - malformed PCWI_PPAR struct array'
		stat = 1
	endif else begin
		if keyword_set(initialized) then begin
			if ppar.initialized ne 1 then begin
				print,pre+': Error - PCWI_PPAR struct not initialized'
				stat = 1
			endif
		endif
	endelse
	;
	return,stat
end
