; $Id: pcwi_group_flats.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GROUP_FLATS
;
; PURPOSE:
;	This procedure groups flats in the PCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_GROUP_FLATS, Pcfg, Ppar, Fcfg
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given directory
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Fcfg	- a PCWI_CFG struct vector with one entry for each flat group
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each flat group.
;
; PROCEDURE:
;	Finds flat images by inspecting the imgtype tags in Pcfg and
;	groups contiguous flat images.  Returns a PCWI_CFG struct vector
;	with one element for each flat group which is used to associate 
;	the flat groups with other observations.
;
; EXAMPLE:
;	Group flat images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = PCWI_READ_CFGS('night1/')
;	PCWI_GROUP_FLATS, KCFG, PPAR, FCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-29	Initial version
;	2013-SEP-09	Added loglun keyword
;	2013-SEP-13	Now use PCWI_PPAR struct for parameters
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_group_flats, pcfg, ppar, fcfg
	;
	; setup
	pre = 'PCWI_GROUP_FLATS'
	;
	; instantiate and init a PCWI_CFG struct for the flat groups
	F = {pcwi_cfg}
	fcfg = struct_init(F)
	;
	; check input
	if pcwi_verify_cfg(pcfg) ne 0 then return
	if pcwi_verify_ppar(ppar) ne 0 then return
	;
	; get flat list
	flats = where(strpos(pcfg.imgtype,'cflat') ge 0, nflats)
	;
	; if we have flats, group them
	if nflats gt 0 then begin
		;
		; create range list of all flats
		rangepar,flats,flist
		;
		; get flat groups split by comma
		fgroups = strsplit(flist,',',/extract,count=ngroups)
		;
		; record number of groups
		ppar.nfgrps = ngroups
		ppar.flatexists = 1
		;
		; setup PCWI_CFG struct for groups
		fcfg = replicate(fcfg, ngroups)
		;
		; loop over flat groups
		for i=0,ngroups-1 do begin
			;
			; fresh copy of PCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			rangepar,fgroups[i],flist
			nims = n_elements(flist)
			imnums = pcfg[flist].imgnum
			rangepar,imnums,rl
			;
			; set parameters
			pp.cflats		= rl
			fcfg[i].grouplist	= rl
			fcfg[i].nimages		= nims
			;
			; get date and coords from first flat in series
			f = flist[0]
			fcfg[i].juliandate	= pcfg[f].juliandate
			fcfg[i].date		= pcfg[f].date
			fcfg[i].ra		= pcfg[f].ra
			fcfg[i].dec		= pcfg[f].dec
			;
			; configuration
			fcfg[i].imgtype		= 'cflat'
			fcfg[i].naxis		= pcfg[f].naxis
			fcfg[i].naxis1		= pcfg[f].naxis1
			fcfg[i].naxis2		= pcfg[f].naxis2
			fcfg[i].binning		= pcfg[f].binning
			fcfg[i].xbinsize	= pcfg[f].xbinsize
			fcfg[i].ybinsize	= pcfg[f].ybinsize
			fcfg[i].ampmode		= pcfg[f].ampmode
			fcfg[i].nasmask		= pcfg[f].nasmask
			fcfg[i].gratid		= pcfg[f].gratid
			fcfg[i].gratpos		= pcfg[f].gratpos
			fcfg[i].filter		= pcfg[f].filter
			fcfg[i].fm4pos		= pcfg[f].fm4pos
			fcfg[i].campos		= pcfg[f].campos
			fcfg[i].focpos		= pcfg[f].focpos
			;
			; use first image number in group
			fi = pcfg[f].imgnum
			;
			; files and directories
			pp.masterflat		= 'mflat_' + $
				string(fi,'(i0'+strn(pp.fdigits)+')') + '.fits'
			pp.ppfname		= 'mflat_' + $
				string(fi,'(i0'+strn(pp.fdigits)+')') + '.ppar'
			;
			fcfg[i].groupnum	= fi
			fcfg[i].groupfile	= pp.masterflat
			fcfg[i].grouppar	= pp.ppfname
			;
			; status
			pp.initialized		= 1
			pp.progid		= pre
			fcfg[i].initialized	= 1
			;
			; write out ppar file
			pcwi_write_ppar,pp
		endfor	; loop over flat groups
	endif else $
		pcwi_print_info,ppar,pre,'no flat frames found',/warning
	;
	return
end
