; $Id: pcwi_set_geom.pro | Tue Apr 21 10:32:09 2015 -0700 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_SET_GEOM
;
; PURPOSE:
;	This procedure uses the input PCWI_CFG struct to set the basic
;	parameters in the PCWI_GEOM struct.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_SET_GEOM, Pgeom, Pcfg
;
; INPUTS:
;	Pgeom	- Input PCWI_GEOM struct.
;	Pcfg	- Input PCWI_CFG struct for a given observation.
;	Ppar	- Input PCWI_PPAR struct.
;
; KEYWORDS:
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Sets the following tags in the PCWI_GEOM struct according to the
;	configuration settings in PCWI_CFG.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-13	Initial version
;	2014-AUG-14	Added CWI Yellow grating
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_set_geom,pgeom,ipcfg,ppar, help=help
	;
	; setup
	pre = 'PCWI_SET_GEOM'
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', Pgeom, Pcfg, Ppar'
		return
	endif
	;
	; verify Pgeom
	ksz = size(pgeom)
	if ksz[2] eq 8 then begin
		if pgeom.initialized ne 1 then begin
			print,pre+': Error - PCWI_GEOM struct not initialized.'
			return
		endif
	endif else begin
		print,pre+': Error - malformed PCWI_GEOM struct'
		return
	endelse
	;
	; verify Pcfg
	if pcwi_verify_cfg(ipcfg,/silent) ne 0 then begin
		print,pre+': Error - malformed PCWI_CFG struct'
		return
	endif
	;
	; verify Ppar
	psz = size(ppar)
	if psz[2] eq 8 then begin
		if ppar.initialized ne 1 then begin
			print,pre+': Error - PCWI_PPAR struct not initialized.'
			return
		endif
	endif else begin
		print,pre+': Error - malformed PCWI_PPAR struct'
		return
	endelse
	;
	; take singleton of PCWI_CFG
	pcfg = ipcfg[0]
	;
	; check image type
	if strtrim(strupcase(pcfg.imgtype),2) ne 'CBARS' then begin
		pcwi_print_info,ppar,pre,'cbars images are the geom reference files, this file is of type',pcfg.imgtype,/error
		return
	endif
	;
	; get output geom file name
	odir = ppar.reddir
	pgeom.geomfile = ppar.reddir + $
	    strmid(pcfg.obsfname,0,strpos(pcfg.obsfname,'_int')) + '_geom.save'
    	;
    	; set basic configuration parameters
	pgeom.gratid = pcfg.gratid
	pgeom.gratnum = pcfg.gratnum
	pgeom.filter = pcfg.filter
	pgeom.filtnum = pcfg.filtnum
	pgeom.campos = pcfg.campos
	pgeom.gratpos = pcfg.gratpos
	pgeom.gratanom = pcfg.gratanom
	pgeom.xbinsize = pcfg.xbinsize
	pgeom.ybinsize = pcfg.ybinsize
	pgeom.nx = pcfg.naxis1
	pgeom.ny = pcfg.naxis2
	pgeom.x0out = 30 / pgeom.xbinsize
	pgeom.goody0 = 10
	pgeom.goody1 = pgeom.ny - 10
	pgeom.trimy0 = 0
	pgeom.trimy1 = pgeom.ny
	pgeom.ypad = 600
	pgeom.nasmask = pcfg.nasmask
	if pcfg.nasmask eq 1 then begin
		pgeom.goody0 = pcfg.nsobjr0 + 18
		pgeom.goody1 = pcfg.nsobjr1 - 18
		pgeom.trimy0 = pcfg.nsobjr0 - 18
		pgeom.trimy1 = pcfg.nsobjr1 + 18
		pgeom.ypad = 0
	endif
	;
	; get noise model
	rdnoise = 0.
	;
	; sum over amp inputs
	switch pcfg.nvidinp of
		4: rdnoise = rdnoise + pcfg.biasrn4
		3: rdnoise = rdnoise + pcfg.biasrn3
		2: rdnoise = rdnoise + pcfg.biasrn2
		1: rdnoise = rdnoise + pcfg.biasrn1
	endswitch
	;
	; take average
	rdnoise /= float(pcfg.nvidinp)
	pgeom.rdnoise = rdnoise
	;
	; wavelength numbers default from header
	pgeom.cwave = pcfg.cwave
	pgeom.wave0out = pcfg.wave0	
	pgeom.wave1out = pcfg.wave1
	pgeom.dwout = pcfg.dwav
	;
	; reference spectrum
	pgeom.refspec = ppar.datdir+ppar.atlas
	pgeom.reflist = ppar.datdir+ppar.linelist
	pgeom.refname = ppar.atlasname
	;
	; default to no cc offsets
	pgeom.ccoff = fltarr(24)
	;
	; check for CWI data
    	if strtrim(strupcase(pcfg.instrume),2) eq 'PCWI' or $
	   strtrim(strupcase(pcfg.instrume),2) eq 'CWI' then begin
		;
		; check resolution and dispersion
		if strtrim(pcfg.gratid,2) eq 'RED' then begin
			pgeom.resolution = 1.16	; Angstroms
			pgeom.wavran = 740.	; Angstroms
			pgeom.ccwn = 260./pgeom.ybinsize	; Pixels
			pgeom.rho = 2.1730d
			pgeom.slant = -1.0d
			pgeom.lastdegree = 4
			;
			; output disperison
			pgeom.dwout = 0.11 * float(pcfg.ybinsize)
		endif else if strtrim(pcfg.gratid,2) eq 'YELLOW' then begin
			pgeom.resolution = 0.82	; Angstroms
			pgeom.wavran = 570.	; Angstroms
			pgeom.ccwn = 260./pgeom.ybinsize	; Pixels
			pgeom.rho = 2.5300d
			pgeom.slant = -1.1d
			pgeom.lastdegree = 4
			;
			; output disperison
			pgeom.dwout = 0.137 * float(pcfg.ybinsize)
		endif else if strtrim(pcfg.gratid,2) eq 'BLUE' then begin
			pgeom.resolution = 0.98	; Angstroms
			pgeom.wavran = 440.	; Angstroms
			pgeom.ccwn = 260./pgeom.ybinsize	; Pixels
			pgeom.rho = 3.050d
			pgeom.slant = 0.50d
			pgeom.lastdegree = 4
			;
			; output disperison
			pgeom.dwout = 0.095 * float(pcfg.ybinsize)
		endif else if strtrim(pcfg.gratid,2) eq 'MEDREZ' then begin
			;
			; MEDREZ requires input offsets or bar-to-bar cc will fail
			offs = [-278.,  -59., -237.,  -32., -216.,  -37., $
				-197.,  -15., -190.,  -20., -175.,    0., $
				   0., -167.,    3., -166.,    3., -156., $
				   5., -173.,  -20., -225.,  -44., -226.]
			pgeom.ccoff = offs
			pgeom.resolution = 2.50	; Angstroms
			pgeom.wavran = 1310.	; Angstroms
			pgeom.ccwn = 80./pgeom.ybinsize	; Pixels
			pgeom.rho = 1.20d
			pgeom.slant = 0.0d
			pgeom.lastdegree = 5
			;
			; output disperison
			pgeom.dwout = 0.275 * float(pcfg.ybinsize)
		endif
		;
		; check central wavelength
		if pgeom.cwave le 0. then $
			pgeom.cwave = cwi_central_wave(strtrim(pgeom.gratid,2),$
				pcfg.campos, pcfg.gratpos)
		;
		; spatial scales
		pgeom.pxscl = 0.00008096d0	; degrees per unbinned pixel
		pgeom.slscl = 0.00075437d0	; degrees per slice
	endif else begin
		pcwi_print_info,ppar,pre,'Unknown instrument',pcfg.instrume,/error
		return
	endelse
	;
	; now check ppar values which override defaults
	if ppar.dw gt 0. then $
		pgeom.dwout = ppar.dw
	if ppar.wave0 gt 0. then $
		pgeom.wave0out = ppar.wave0
	if ppar.wave1 gt 0. then $
		pgeom.wave1out = ppar.wave1
	;
	; print log of values
	pcwi_print_info,ppar,pre,'Data cube output Disp (A/px), Wave0 (A): ', $
		pgeom.dwout,pgeom.wave0out,format='(a,f8.3,f9.2)'
	;
	return
end
