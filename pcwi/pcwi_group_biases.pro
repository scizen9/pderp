; $Id: pcwi_group_biases.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GROUP_BIASES
;
; PURPOSE:
;	This procedure groups biases in the PCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_GROUP_BIASES, Pcfg, Ppar, Bcfg
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given directory
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Bcfg	- a PCWI_CFG struct vector with one entry for each bias group
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each bias group.
;
; PROCEDURE:
;	Finds bias images by inspecting the imgtype tags in Pcfg and
;	groups contiguous bias images.  Returns a PCWI_CFG struct vector
;	with one element for each bias group which is used to associate 
;	the bias groups with other observations.
;
; EXAMPLE:
;	Group bias images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = PCWI_READ_CFGS('night1/')
;	PCWI_GROUP_BIASES, KCFG, PPAR, BCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-03	Initial version
;	2013-SEP-09	Added loglun keyword
;	2013-SEP-13	Now use PCWI_PPAR struct for parameters
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_group_biases, pcfg, ppar, bcfg
	;
	; setup
	pre = 'PCWI_GROUP_BIASES'
	;
	; instantiate and init a PCWI_CFG struct for the bias groups
	B = {pcwi_cfg}
	bcfg = struct_init(B)
	;
	; check inputs
	if pcwi_verify_cfg(pcfg) ne 0 then return
	if pcwi_verify_ppar(ppar) ne 0 then return
	;
	; get bias list
	biases = where(strpos(pcfg.imgtype,'bias') ge 0, nbiases)
	;
	; if we have biases, group them
	if nbiases gt 0 then begin
		;
		; create range list of all biases
		rangepar,biases,blist
		;
		; get bias groups split by comma
		bgroups = strsplit(blist,',',/extract,count=ngroups)
		;
		; record number of groups
		ppar.nbgrps = ngroups
		ppar.biasexists = 1
		;
		; setup PCWI_CFG struct for groups
		bcfg = replicate(bcfg, ngroups)
		;
		; loop over bias groups
		g = 0	; good group counter
		for i=0,ngroups-1 do begin
			;
			; fresh copy of PCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			rangepar,bgroups[i],blist
			nims = n_elements(blist)
			;
			; check if skip1 set
			if pp.biasskip1 ne 0 and nims gt 1 then begin
				blist = blist[1:*]
				nims = n_elements(blist)
			endif
			;
			; do we have enough for a group?
			if nims ge pp.mingroupbias then begin
				imnums = pcfg[blist].imgnum
				rangepar,imnums,rl
				pp.biases		= rl
				bcfg[g].grouplist	= rl
				bcfg[g].nimages		= nims
				;
				; get date from first bias in series
				b = blist[0]
				bcfg[g].juliandate	= pcfg[b].juliandate
				bcfg[g].date		= pcfg[b].date
				;
				; configuration
				bcfg[g].imgtype		= 'bias'
				bcfg[g].naxis		= pcfg[b].naxis
				bcfg[g].naxis1		= pcfg[b].naxis1
				bcfg[g].naxis2		= pcfg[b].naxis2
				bcfg[g].binning		= pcfg[b].binning
				bcfg[g].xbinsize	= pcfg[b].xbinsize
				bcfg[g].ybinsize	= pcfg[b].ybinsize
				;
				; use first image number in group
				gi = pcfg[b].imgnum
				;
				; files and directories
				pp.masterbias		= 'mbias_' + $
					string(gi,'(i0'+strn(pp.fdigits)+')') +$
								'.fits'
				pp.ppfname		= 'mbias_' + $
					string(gi,'(i0'+strn(pp.fdigits)+')') +$
								'.ppar'
				bcfg[g].groupnum	= gi
				bcfg[g].groupfile	= pp.masterbias
				bcfg[g].grouppar	= pp.ppfname
				;
				; status
				pp.initialized		= 1
				pp.progid		= pre
				bcfg[g].initialized	= 1
				;
				; write out ppar file
				pcwi_write_ppar,pp
				;
				; increment group counter
				g = g + 1
			endif	; do we have enough images?
		endfor	; loop over bias groups
		;
		; all groups failed
		if g le 0 then begin
			;
			; return an uninitialized, single PCWI_CFG struct
			bcfg = bcfg[0]
			ppar.biasexists = 0
			pcwi_print_info,ppar,pre,'no bias groups with >= ', $
				ppar.mingroupbias, ' images.',/warning
		;
		; some groups failed
		endif else if g lt ngroups then begin
			;
			; trim PCWI_CFG struct to only good groups
			bcfg = bcfg[0:(g-1)]
			pcwi_print_info,ppar,pre,'removing ', ngroups - g, $
				' bias groups with < ', ppar.mingroupbias, $
				' images.', format='(a,i3,a,i3,a)'
		endif	; otherwise, we are OK as is
		;
		; update number of groups
		ppar.nbgrps = g
	;
	; no bias frames found
	endif else $
		pcwi_print_info,ppar,pre,'no bias frames found',/warning
	;
	return
end
