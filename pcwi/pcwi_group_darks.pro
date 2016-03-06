; $Id: pcwi_group_darks.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GROUP_DARKS
;
; PURPOSE:
;	This procedure groups biases in the PCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_GROUP_DARKS, Pcfg, Ppar, Dcfg
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given night
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Dcfg	- array of struct PCWI_CFG, one for each dark group
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each dark group.
;
; PROCEDURE:
;	Finds dark images by inspecting the imgtype tags in Pcfg and
;	gropus contiguous dark images.  Returns a PCWI_CFG struct vector
;	with one element for each dark group which is used to associate 
;	the dark groups with other observations.
;
; EXAMPLE:
;	Group dark images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = PCWI_NIGHT_READ('night1/')
;	PCWI_GROUP_DARKS, KCFG, PPAR, DCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-03	Initial version
;	2013-SEP-09	Added loglun keyword
;	2013-SEP-13	Now use PCWI_PPAR struct for parameters
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_group_darks, pcfg, ppar, dcfg
	;
	; setup
	pre = 'PCWI_GROUP_DARKS'
	;
	; instantiate and init a PCWI_CFG struct for the dark groups
	D = {pcwi_cfg}
	dcfg = struct_init(D)
	;
	; check inputs
	if pcwi_verify_cfg(pcfg) ne 0 then return
	if pcwi_verify_ppar(ppar) ne 0 then return
	;
	; get dark list
	darks = where(strmatch(pcfg.imgtype,'dark') eq 1, ndarks)
	;
	; if we have darks, group them
	if ndarks gt 0 then begin
		;
		; create range list of all darks
		rangepar,darks,dlist
		;
		; get dark groups split by comma
		dgroups = strsplit(dlist,',',/extract,count=ngroups)
		;
		; record number of groups
		ppar.ndgrps = ngroups
		ppar.darkexists = 1
		;
		; setup PCWI_CFG struct for groups
		dcfg = replicate(dcfg, ngroups)
		;
		; loop over dark groups
		g = 0	; good group counter
		for i=0,ngroups-1 do begin
			;
			; fresh copy of PCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			rangepar,dgroups[i],dlist
			nims = n_elements(dlist)
			;
			; do we have enough for a group?
			if nims ge pp.mingroupdark then begin
				imnums = pcfg[dlist].imgnum
				rangepar,imnums,rl
				pp.darks		= rl
				dcfg[g].grouplist	= rl
				dcfg[g].nimages		= nims
				;
				; get date from first dark in series
				d = dlist[0]
				dcfg[g].juliandate	= pcfg[d].juliandate
				dcfg[g].date		= pcfg[d].date
				;
				; configuration
				dcfg[g].imgtype		= 'dark'
				dcfg[g].naxis		= pcfg[d].naxis
				dcfg[g].naxis1		= pcfg[d].naxis1
				dcfg[g].naxis2		= pcfg[d].naxis2
				dcfg[g].binning		= pcfg[d].binning
				dcfg[g].xbinsize	= pcfg[d].xbinsize
				dcfg[g].ybinsize	= pcfg[d].ybinsize
				;
				; use first image number in group
				gi = pcfg[d].imgnum
				;
				; files and directories
				pp.masterdark		= 'mdark_' + $
					string(gi,'(i0'+strn(pp.fdigits)+')') +$
								'.fits'
				pp.ppfname		= 'mdark_' + $
					string(gi,'(i0'+strn(pp.fdigits)+')') +$
								'.ppar'
				;
				dcfg[g].groupnum	= gi
				dcfg[g].groupfile	= pp.masterdark
				dcfg[g].grouppar	= pp.ppfname
				;
				; status
				pp.initialized		= 1
				pp.progid		= pre
				dcfg[g].initialized	= 1
				;
				; write out ppar file
				pcwi_write_ppar,pp
				;
				; increment group counter
				g = g + 1
			endif	; do we have enough images?
		endfor	; loop over dark groups
		;
		; all groups failed
		if g le 0 then begin
			;
			; return an uninitialized, single PCWI_CFG struct
			dcfg = dcfg[0]
			ppar.darkexists = 0
			pcwi_print_info,ppar,pre,'no dark groups with >= ', $
				ppar.mingroupdark, ' images.',/warning
		;
		; some groups failed
		endif else if g lt ngroups then begin
			;
			; trim PCWI_CFG struct to only good groups
			dcfg = dcfg[0:(g-1)]
			pcwi_print_info,ppar,pre,'removing ', ngroups - g, $
				' dark groups with < ', ppar.mingroupdark, $
				' images.', format='(a,i3,a,i3,a)'
		endif	; otherwise, we are OK as is
		;
		; update number of grouops
		ppar.ndgrps = g
	;
	; no dark frames found
	endif else $
		pcwi_print_info,ppar,pre,'no dark frames found',/warning
	;
	return
end
