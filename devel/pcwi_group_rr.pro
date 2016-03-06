; $Id: pcwi_group_rr.pro,v 1.2 2015/02/21 00:18:40 neill Exp $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GROUP_RR
;
; PURPOSE:
;	This procedure groups relative response images in the PCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_GROUP_RR, Pcfg, Ppar, Fcfg
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given directory
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Rcfg	- a PCWI_CFG struct vector with one entry for each rr group
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	Outputs pipeline parameter file in ODIR for each rr group.
;
; PROCEDURE:
;	Finds rr images by inspecting the imgtype tags in Pcfg and
;	groups contiguous rr images.  Returns a PCWI_CFG struct vector
;	with one element for each rr group which is used to associate 
;	the rr groups with other observations.
;
; EXAMPLE:
;	Group rr images from directory 'night1/' and put the resulting
;	ppar files in 'night1/redux/':
;
;	KCFG = PCWI_READ_CFGS('night1/')
;	PCWI_GROUP_RR, KCFG, PPAR, RCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-JUL-29	Initial version
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_group_rr, pcfg, ppar, rcfg
	;
	; setup
	pre = 'PCWI_GROUP_RR'
	version = repstr('$Revision: 1.2 $ $Date: 2015/02/21 00:18:40 $','$','')
	;
	; instantiate and init a PCWI_CFG struct for the rr groups
	R = {pcwi_cfg}
	rcfg = struct_init(R)
	;
	; check input
	if pcwi_verify_cfg(pcfg) ne 0 then return
	if pcwi_verify_ppar(ppar) ne 0 then return
	;
	; get rr list
	rrs = where(strcmp(pcfg.imgtype,'dflat') eq 1, nrrs)
	;
	; if we have rrs, group them
	if nrrs gt 0 then begin
		;
		; create range list of all rrs
		rangepar,rrs,rlist
		;
		; get rr groups split by comma
		rgroups = strsplit(rlist,',',/extract,count=ngroups)
		;
		; record number of groups
		ppar.nrgrps = ngroups
		ppar.rrexists = 1
		;
		; setup PCWI_CFG struct for groups
		rcfg = replicate(rcfg, ngroups)
		;
		; loop over rr groups
		for i=0,ngroups-1 do begin
			;
			; fresh copy of PCWI_PPAR struct
			pp = ppar
			;
			; get image numbers for this group
			rangepar,rgroups[i],rlist
			nims = n_elements(rlist)
			imnums = pcfg[rlist].imgnum
			rangepar,imnums,rl
			;
			; set parameters
			pp.rrs			= rl
			rcfg[i].grouplist	= rl
			rcfg[i].nimages		= nims
			;
			; get date from first rr in series
			r = rlist[0]
			rcfg[i].juliandate	= pcfg[r].juliandate
			rcfg[i].date		= pcfg[r].date
			;
			; configuration
			rcfg[i].imgtype		= 'dflat'
			rcfg[i].naxis		= pcfg[r].naxis
			rcfg[i].naxis1		= pcfg[r].naxis1
			rcfg[i].naxis2		= pcfg[r].naxis2
			rcfg[i].binning		= pcfg[r].binning
			rcfg[i].xbinsize	= pcfg[r].xbinsize
			rcfg[i].ybinsize	= pcfg[r].ybinsize
			rcfg[i].ampmode		= pcfg[r].ampmode
			rcfg[i].nasmask		= pcfg[r].nasmask
			rcfg[i].gratid		= pcfg[r].gratid
			rcfg[i].gratpos		= pcfg[r].gratpos
			rcfg[i].filter		= pcfg[r].filter
			rcfg[i].fm4pos		= pcfg[r].fm4pos
			rcfg[i].campos		= pcfg[r].campos
			rcfg[i].focpos		= pcfg[r].focpos
			;
			; use first image number in group
			ri = pcfg[r].imgnum
			;
			; files and directories
			pp.masterrr		= 'mrr_' + strn(ri) + '.fits'
			pp.ppfname		= 'mrr_' + strn(ri) + '.ppar'
			;
			rcfg[i].groupnum	= ri
			rcfg[i].groupfile	= pp.masterrr
			rcfg[i].grouppar	= pp.ppfname
			;
			; status
			pp.initialized		= 1
			pp.progid		= pre+': '+version
			rcfg[i].initialized	= 1
			;
			; write out ppar file
			pcwi_write_ppar,pp
		endfor	; loop over rr groups
	endif else $
		pcwi_print_info,ppar,pre,'no rr frames found',/warning
	;
	return
end
