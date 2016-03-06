; $Id: pcwi_drp_batch.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_DRP_BATCH
;
; PURPOSE:
;	This procedure will run the pipeline in batch mode.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_DRP_BATCH, DirList
;
; INPUTS:
;	DirList	- list of run directories (string array)
;
; KEYWORDS:
;	DARK		- set to run PCWI_STAGE2DARK (def: NO)
;	CWI		- set to skip first bias and use CWI associations(def: NO)
;	MINOSCANPIX	- set to minimum pixels required for overscan subtraction
;	LASTSTAGE	- set to the last stage you want run
;	ONESTAGE	- set to a single stage you want run (overrides LASTSTAGE)
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Runs pipeline in each directory specified in DirList.
;
; EXAMPLE:
;	PCWI_DRP_BATCH,['140527','140528','140529']
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-JUN-03	Initial version
;	2014-OCT-23	Added onestage keyword
;	2014-NOV-07	Added stage7std to nominal run
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_drp_batch,dirlist,dark=dark, $
	minoscanpix=minoscanpix, $
	laststage=laststage, $
	onestage=onestage
;
; check keywords
if keyword_set(laststage) then $
	last = laststage $
else	last = 7
;
if keyword_set(onestage) then $
	one = onestage $
else	one = 0
;
; how many directories?
ndir = n_elements(dirlist)
;
; get defaults from PCWI_PPAR struct
A = {pcwi_ppar}
ppar = struct_init(A)
;
; loop over directories
for i=0,ndir-1 do begin
	cd,dirlist[i]
	print,dirlist[i]
	;
	; check for one stage
	if one gt 0 then begin
		case one of
			1: pcwi_stage1
			2: pcwi_stage2dark
			3: pcwi_stage3flat
			4: pcwi_stage4geom
			5: pcwi_stage5prof
			6: pcwi_stage6rr
			7: pcwi_stage7std
			else: print,'Illegal stage: ',one
		endcase
	;
	; otherwise run up to last stage
	endif else begin
		;
		; archive any existing output directory
		filestamp,ppar.reddir,/verbose
		;
		; make a new output directory
		spawn,'mkdir '+ppar.reddir
		;
		; get the pipeline ready
		pcwi_prep,/verbose,/display,minoscanpix=minoscanpix
		;
		; do basic ccd image reduction
		pcwi_stage1
		if last le 1 then goto,done
		;
		; if requested do dark subtraction
		if keyword_set(dark) then $
			pcwi_stage2dark
		if last le 2 then goto,done
		;
		; do flat field correction
		pcwi_stage3flat
		if last le 3 then goto,done
		;
		; solve for wavelengths and geometry
		pcwi_stage4geom
		if last le 4 then goto,done
		;
		; do slice profile correction
		pcwi_stage5prof
		if last le 5 then goto,done
		;
		; do relative response correction
		pcwi_stage6rr
		if last le 6 then goto,done
		;
		; do standard star calibration
		pcwi_stage7std
		;
		; done
		done:
	endelse
	;
	; return to where we started
	cd,'..'
endfor	; loop over directories
;
return
end
