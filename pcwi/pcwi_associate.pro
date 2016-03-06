; $Id: pcwi_associate.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_ASSOCIATE
;
; PURPOSE:
;	This function returns the indices of the PCWI_CFG array that
;	is closest in time to the target PCWI_CFG scalar input.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_ASSOCIATE( KCFG, TCFG )
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given group
;	Tcfg	- target scalar struct PCWI_CFG to match
;	Ppar	- pipeline parameters PCWI_PPAR struct
;
; Returns:
;	Index of the Pcfg entry that is closest in time to the target Tcfg
;
; INPUT KEYWORDS:
;	AFTER	- match the closest in time after epoch of target
;	BEFORE	- match the closest in time before epoch of target
;
; OUTPUT KEYWORD:
;	COUNT	- set to get extra screen output
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;	Compares target Julian date given by Tcfg to Julian dates for
;	group contained in Pcfg and finds the entry with the smallest
;	time offset compared to the target.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-08	Initial version
;	2013-OCT-31	Now returns matched PCWI_CFG struct
;	2013-NOV-01	Added AFTER/BEFORE keywords
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_associate, pcfg, tcfg, ppar, $
	after=after, before=before, count=count
	;
	; setup
	pre = 'PCWI_ASSOCIATE'
	count = 0
	;
	; check inputs
	if pcwi_verify_ppar(ppar,/init,/silent) ne 0 then begin
		pcwi_print_info,ppar,pre,'Ppar - malformed PCWI_PPAR struct',/error
		match = -1
	endif
	if pcwi_verify_cfg(pcfg,/init,/silent) ne 0 then begin
		pcwi_print_info,ppar,pre,'Search - malformed PCWI_CFG struct array',/error
		match = -1
	endif
	if pcwi_verify_cfg(tcfg,/init,/silent) ne 0 then begin
		pcwi_print_info,ppar,pre,'Target - malformed PCWI_CFG struct array',/error
		match = -1
	endif
	if tcfg.juliandate le 0. then begin
		pcwi_print_info,ppar,pre,'target date not set',/error
		match = -1
	endif
	if total(pcfg.juliandate) le 0. then begin
		pcwi_print_info,ppar,pre,'group dates not set',/error
		match = -1
	endif
	;
	; check after match
	if keyword_set(after) then begin
		offs = pcfg.juliandate - tcfg.juliandate
		a = where(offs ge 0., na)
		if na gt 0 then begin
			offs = offs[a]
			match = (where(offs eq min(offs)))[0]
		endif else begin
			pcwi_print_info,ppar,pre,'no after match',/error
			match = -1
		endelse
	;
	; check before match
	endif else if keyword_set(before) then begin
		offs = tcfg.juliandate - pcfg.juliandate
		b = where(offs ge 0., nb)
		if nb gt 0 then begin
			offs = offs[b]
			match = (where(offs eq min(offs)))[0]
		endif else begin
			pcwi_print_info,ppar,pre,'no before match',/error
			match = -1
		endelse
	;
	; get offsets
	endif else begin
		offs = abs(pcfg.juliandate - tcfg.juliandate)
		match = (where(offs eq min(offs)))[0]
	endelse
	;
	if match[0] ge 0 then begin
		count = 1
		return,pcfg[match]
	endif else return,match
end
