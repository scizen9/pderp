; $Id: pcwi_find_stds.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_FIND_STDS
;
; PURPOSE:
;	This function finds the standard star observations within
;	the input configuration structure.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_FIND_STDS( Pcfg,  Ppar, Nstds)
;
; INPUTS:
;	Pcfg	- PCWI_CFG struct array
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Nstds	- How many standard star observations were found?
;
; RETURNS:
;	The indices of the observations within Pcfg that are standard star
;	observations.
;
; SIDE EFFECTS:
;	None.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Gets a list of standard star reference spectra in !PCWI_DATA directory
;	and compares the names to the object names in Pcfg configuration
;	struct to determine which are standard star observations.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-NOV-05	Initial Revision
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_find_stds,pcfg,ppar,nstds
	;
	; setup
	pre = 'PCWI_FIND_STDS'
	q=''
	nstds = 0
	stds = -1
	;
	; check inputs
	if pcwi_verify_cfg(pcfg,/init) ne 0 then return,stds
	if pcwi_verify_ppar(ppar,/init) ne 0 then return,stds
	;
	; log
	pcwi_print_info,ppar,pre,systime(0)
	;
	; directories
	if pcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		pcwi_print_info,ppar,pre,'Directory error, returning',/error
		return,stds
	endif
	;
	; test standard star directory
	if file_test(ddir+'stds',/directory,/read) ne 1 then begin
		pcwi_print_info,ppar,pre,'Standard star reference dir inaccessable, returning',/error
		return,stds
	endif
	;
	; get list of standard star reference spectra
	reflist = file_search(ddir+'stds/*.fit*',count=nrefs)
	;
	; get names from file names
	for i=0,nrefs-1 do begin
		fdecomp,reflist[i],disk,dir,name,ext
		reflist[i] = name
	endfor
	;
	; get observation names
	obnames = strlowcase(strtrim(pcfg.object,2))
	obstat = strcmp(strtrim(pcfg.imgtype,2),'object')
	;
	; set up a status array
	stdstat = intarr(n_elements(pcfg))
	;
	; loop over reference list
	for i=0,nrefs-1 do begin
		t = where(obstat eq 1 and strcmp(obnames,reflist[i]) eq 1, nt)
		if nt gt 0 then stdstat[t] = 1
	endfor
	;
	; get the standards, if any
	stds = where(stdstat eq 1, nstds)
	;
	; log results
	pcwi_print_info,ppar,pre,'Found this many standard star observations', $
		nstds,form='(a,i4)'
	;
	return,stds
end	; pcwi_find_stds
