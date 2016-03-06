; $Id: pcwi_verify_cfg.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_VERIFY_CFG
;
; PURPOSE:
;	This function verifies the input PCWI_CFG struct.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_VERIFY_CFG(Pcfg)
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG
;
; RETURNS:
;	The status of the input PCWI_CFG struct:
;	0	- verified without problems
;	1	- a malformed PCWI_CFG struct was passed
;
; KEYWORDS:
;	INITIALIZED - set to check if PCWI_CFG is initialized
;	SILENT	- set to silence output
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-08	Initial version
;	2013-SEP-14	Added initialized keyword
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_verify_cfg,pcfg,initialized=initialized,silent=silent
	;
	; setup
	pre = 'PCWI_VERIFY_CFG'
	;
	; check input
	stat = 0
	sz = size(pcfg)
	if sz[0] ne 1 or sz[1] lt 1 or sz[2] ne 8 then begin
		if not keyword_set(silent) then $
			print,pre+': Error - malformed PCWI_CFG struct array'
		stat = 1
	endif else begin
		if keyword_set(initialized) then begin
			test = n_elements(pcfg)
			if total(pcfg.initialized) ne test then begin
				print,pre+': Error - PCWI_CFG struct not initialized'
				stat = 1
			endif
		endif
	endelse
	;
	return,stat
end
