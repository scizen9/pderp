; $Id: pcwi_group_geom.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GROUP_GEOM
;
; PURPOSE:
;	This procedure groups continuum bars (cbars) and arcs in the PCWI_CFG 
;	struct for a given night.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_GROUP_GEOM, Pcfg, Ppar, Ccfg, Acfg, Ngeom
;
; INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given directory
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Ccfg	- a PCWI_CFG struct vector with one entry for each cbars from
;			a calibration set
;	Acfg	- a PCWI_CFG struct vector with one entry for each arc from
;			a calibration set
;	Ngeom	- number of good geometry calibration groups
;
; KEYWORDS:
;
; SIDE EFFECTS:
;	None.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-01	Initial version
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_group_geom, pcfg, ppar, ccfg, acfg, ngeom
	;
	; setup
	pre = 'PCWI_GROUP_GEOM'
	;
	; check input
	if pcwi_verify_cfg(pcfg) ne 0 then return
	if pcwi_verify_ppar(ppar) ne 0 then return
	;
	; get size of pcfg
	ncfg = n_elements(pcfg)
	;
	; calibration groups
	cg = where(pcfg.obstype eq 'cal', ncal)
	;
	; set up for group counting
	ngeom = 0
	maxgrps = 100
	maxmemb = 50
	groups = lonarr(maxgrps,maxmemb) - 1l
	gind = 0
	p = 0
	;
	; set up first group
	gcfg = pcfg[cg[0]]
	groups[gind,p] = cg[0]
	p += 1
	;
	; loop over cal images and gather groups
	for i=1,ncal-1 do begin
		;
		; check configuration
		tcfg = pcwi_match_cfg(pcfg[cg[i]],gcfg,ppar,count=nm,/silent)
		;
		; check for sequential image numbers
		if pcfg[cg[i]].imgnum - gcfg.imgnum ne 1 or nm ne 1 then begin
			;
			; new group
			gind += 1
			p = 0
			;
			; check for group overflow
			if gind ge maxgrps then begin
				pcwi_print_info,'geom group overflow',gind,/error
				return
			endif
			;
			; first member of group
			gcfg = pcfg[cg[i]]
			groups[gind,p] = cg[i]
			p += 1
			;
			; check for member overflow
			if p ge maxmemb then begin
				pcwi_print_info,'geom group member overflow',p,/error
				return
			endif
		endif else begin
			;
			; next member of group
			gcfg = pcfg[cg[i]]
			groups[gind,p] = cg[i]
			p += 1
			;
			; check for member overflow
			if p ge maxmemb then begin
				pcwi_print_info,'geom group member overflow',p,/error
				return
			endif
		endelse
	endfor
	;
	; number of groups
	ngeom = gind + 1
	;
	; we'll check the status of each group
	stat = intarr(ngeom)
	;
	; here's where we collect arc and cbars indices
	ari = lonarr(ngeom) - 1l
	cbi = lonarr(ngeom) - 1l
	;
	; loop over groups
	for i=0,ngeom-1 do begin
		;
		; get indexes for this group
		igrp = reform(groups[i,*])
		good = where(igrp ge 0, nmem)
		igrp = igrp[good]
		;
		; look for arc/cbars nearest pair
		arci = -1l
		cbri = -1l
		for j=0,nmem-1 do begin
			;
			; collect each arc and cbars image in the group
			if strtrim(pcfg[igrp[j]].imgtype,2) eq 'arc' then $
				arci = [arci, igrp[j]]
			if strtrim(pcfg[igrp[j]].imgtype,2) eq 'cbars' then $
				cbri = [cbri, igrp[j]]
		endfor
		;
		; how many do we have?
		ga = where(arci ge 0, nga)
		gc = where(cbri ge 0, ngc)
		;
		; do we have enough?
		if nga gt 0 and ngc gt 0 then begin
			cbri = cbri[gc]
			arci = arci[ga]
			;
			; now find closest pair
			cbrimn = pcfg[cbri].imgnum
			arcimn = pcfg[arci].imgnum
			;
			one_cbr = intarr(ngc) + 1
			one_arc = intarr(nga) + 1
			diff = abs( (arcimn##one_cbr) - (one_arc##cbrimn))
			;
			match = (where(diff eq min(diff)))[0]
			ind = array_indices(diff,match)
			;
			cbi[i] = cbri[ind[0]]
			ari[i] = arci[ind[1<(n_elements(ind)-1)]]
			stat[i] = 1
		endif
	endfor
	;
	; get the good ones
	good = where(stat eq 1, ngeom)
	;
	; collect the good calibs
	if ngeom gt 0 then begin
		;
		; arcs
		acfg = pcfg[ari[good]]
		;
		; cbars
		ccfg = pcfg[cbi[good]]
	endif else begin
		acfg = -1
		ccfg = -1
		pcwi_print_info,ppar,pre,'no geom frame sets found',/warning
	endelse
	;
	; record results
	ppar.ncbars = ngeom
	ppar.narcs = ngeom
	if ngeom gt 0 then ppar.geomexists = 1
	;
	return
end
