; $Id: pcwi_group_obs.pro,v 1.2 2015/02/21 00:18:39 neill Exp $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GROUP_OBS
;
; PURPOSE:
;	This procedure groups objects in the PCWI_CFG struct for a given night.
;
; CATEGORY:
;	Data reduction for the Keck Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_GROUP_OBS, KCFG, INDIR, ODIR, OCFG
;
; OPTIONAL INPUTS:
;	Pcfg	- array of struct PCWI_CFG for a given directory
;	Indir	- input directory (string) defaults to current dir
;	Odir	- output directory (string) defaults to current dir
;
; OUTPUTS:
;	Ocfg	- a PCWI_CFG struct vector with one entry per object image
;
; KEYWORDS:
;	COUNT	- number of good groups found
;	VERBOSE	- set to get extra screen output
;
; SIDE EFFECTS:
;	outputs pipeline parameter file in ODIR for each object group.
;
; PROCEDURE:
;	Finds object images by inspecting the imgtype tags in Pcfg and
;	groups with nearby (in time) calibration images.  Returns a PCWI_CFG
;	struct vector with one element for each object group which is used 
;	to associate the object groups with calibration files.
;
; EXAMPLE:
;	Group image data in directory 'night1' and put resulting pipeline
;	parameter files in 'night1/redux':
;
;	KCFG = PCWI_READ_CFGS('night1/')
;	PCWI_GROUP_OBS, KCFG, 'night1/', 'night1/redux/', OCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-16	Initial version
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_group_obs, pcfg, indir, odir, ocfg, count=count, $
	filespec=filespec, verbose=verbose
	;
	; setup
	pre = 'PCWI_GROUP_OBS'
	;
	; check inputs
	if pcwi_verify_cfg(pcfg) ne 0 then return
	;
	; instantiate and init a PCWI_CFG struct for the object groups
	O = {pcwi_cfg}
	ocfg = struct_init(O)
	count = 0
	;
	; instantiate a and init ppar structure for the pipeline parameters
	A = {pcwi_ppar}
	ppar = struct_init(A)
	;
	; get object list
	objects = where(strpos(pcfg.imgtype,'object') ge 0, nobj)
	;
	; test object names
	good = where(strlen(strtrim(pcfg[objects].object,2)) gt 0, ngood)
	if ngood le 0 then begin
		print,pre+': Header Keyword (OBJECT) Error - no valid objects'
		return
	endif
	;
	; get unique object id's
	sobs = strtrim(pcfg[good].object,2)
	sobs = sobs[sort(sobs)]
	sobs = sobs[uniq(sobs)]
	nuobs = n_elements(sobs)
	;
	; maintain a list of unique object/configuration groups
	uocgrps = ['']
	uocpars = [ppar]
	;
	; loop over each unique id
	for i=0,nuobs-1 do begin
		;
		; this object's observations
		t=where(strmatch(strtrim(pcfg.object,2),sobs[i]) eq 1 and $
			strmatch(strtrim(pcfg.imgtype,2),'object') eq 1, nt)
		if nt gt 0 then begin
			;
			; loop over this object's observations
			pnum = 0
			skynum = 0
			for j=0,nt-1 do begin
				p = t[j]	; pointer into config struct array
				;
				; this allows us to skip grouped observations of same object
				if done[p] eq 0 then begin
					;
					; new copy of pipeline parameter struct
					pp = ppar
					;
					; output file name
					pp.ppfname = sobs[i]+'_'+strn(pnum)+'.ppar'
					;
					; set observation parameters
					pp.object	= pcfg[p].object
					pp.observer	= pcfg[p].observer
					pp.imgtype	= 'object'
					pp.date		= pcfg[p].date
					pp.juliandate	= pcfg[p].juliandate
					pp.rawdir	= indir
					pp.reddir	= odir
					;
					; group observations of same object by configuration
					same = pcwi_match_cfg(pcfg,pcfg[p],object=sobs[i],count=nsame)
					;
					; mark object/config group completed
					done[same] = 1
					;
					; are there one or more sky observations?
					sky = where(pcfg[same].skyobs ne 0, nsky)
					if nsky gt 0 then begin
						pp.skyexists = 1
						rangepar,pcfg[same[sky]].imgnum,rl
						pp.sky = rl
						if nsky lt nsame then $
							remove,sky,same
					endif else begin
						pp.skyexists = 0
						pp.sky = ''
						if pcfg[p].shuffmod eq 0 then $
							print,pre+': Warning - no sky frame for: ',pp.ppfname
					endelse
					rangepar,pcfg[same].imgnum,rl
					pp.npims = n_elements(same)
					pp.imnum = rl
					;
					; record configuration
					pp.binning 	= pcfg[p].binning
					pp.xbinsize	= pcfg[p].xbinsize
					pp.ybinsize	= pcfg[p].ybinsize
					pp.shuffmod	= pcfg[p].shuffmod
					pp.nasmask	= pcfg[p].nasmask
					pp.gratid	= pcfg[p].gratid
					pp.gratpos	= pcfg[p].gratpos
					pp.filter	= pcfg[p].filter
					pp.fm4pos	= pcfg[p].fm4pos
					pp.campos	= pcfg[p].campos
					pp.focpos	= pcfg[p].focpos
					;
					; dark and bias lists
					;
					; check biases
					if nbgrps gt 0 then begin
						b = pcwi_associate(bcfg,pcfg[p])
						if b ge 0 then begin
							pp.biasexists	= 1
							pp.biases	= bcfg[b].grouplist
							pp.masterbias	= bcfg[b].groupfile
						endif
					endif
					;
					; check darks
					if ndgrps gt 0 then begin
						d = pcwi_associate(dcfg,pcfg[p])
						if d ge 0 then begin
							pp.darkexists	= 1
							pp.darks	= dcfg[d].grouplist
							pp.masterdark	= dcfg[d].groupfile
						endif
					endif
					;
					; group continuum flats by configuration
					cflats = pcwi_match_cfg(pcfg,pcfg[p],imgtype='cflat',count=ncflats)
					if ncflats gt 0 then begin
						rangepar,pcfg[cflats].imgnum,rl
						pp.cflats = rl
						pp.flatexists = 1
					endif else begin
						pp.cflats = ''
						pp.flatexists = 0
						print,pre+': Warning - no continuum flats for: ', $
							pp.ppfname
					endelse
					;
					; group continuum bars by configuration
					cbars = pcwi_match_cfg(pcfg,pcfg[p],imgtype='cbars',count=ncbars)
					if ncbars gt 0 then begin
						rangepar,pcfg[cbars].imgnum,rl
						pp.cbars = rl
					endif else begin
						pp.cbars = ''
						print,pre+': Warning - no continuum bars for: ', $
							pp.ppfname
					endelse
					;
					; group arcs by configuration
					arcs = pcwi_match_cfg(pcfg,pcfg[p],imgtype='arc',count=narcs)
					if narcs gt 0 then begin
						rangepar,pcfg[arcs].imgnum,rl
						pp.arcs = rl
					endif else begin
						pp.arcs = ''
						print,pre+': Warning - no arcs for: ',pp.ppfname
					endelse
					;
					; group arc bars by configuration
					arcbars = pcwi_match_cfg(pcfg,pcfg[p],imgtype='arcbars',count=narcbars)
					if narcbars gt 0 then begin
						rangepar,pcfg[arcbars].imgnum,rl
						pp.arcbars = rl
					endif else begin
						pp.arcbars = ''
						print,pre+': Warning - no arc bars for: ', $
							pp.ppfname
					endelse
					;
					; we have initialized the ppar structure
					pp.initialized	= 1
					;
					; check for sky obs
					if nsky gt 0 then begin
						ppsky = pp
						ppsky.npims = nsky
						ppsky.imnum = ppsky.sky
						ppsky.ppfname = sobs[i]+'_sky_'+strn(skynum)+'.ppar'
						;
						; record in list
						grpsky = string(ppsky.object,ppsky.npims,ppsky.gratid,ppsky.filter, $
						     ppsky.shuffmod,ppsky.nasmask,1, $
						     ppsky.fm4pos,ppsky.gratpos,ppsky.campos,ppsky.focpos, $
						     ppsky.biasexists,ppsky.darkexists,ppsky.flatexists, $
						     ppsky.skyexists,ppsky.binning,ppsky.xbinsize,ppsky.ybinsize, $
						     ppsky.ppfname, $
					format = '(a-16,1x,i3,1x,a4,1x,a4,1x,3i1,1x,4i7,1x,4i1,1x,3i1,1x,a)')
						skynum = skynum + 1
						uocgrps = [ uocgrps, grpsky ]
						uocpars = [ uocpars, ppsky ]
					endif
					;
					; do we still have regular observations after removing the skies?
					if nsky lt nsame then begin
						pp.ppfname = sobs[i]+'_'+strn(pnum)+'.ppar'
						;
						; record in list
						grp = string(pp.object,pp.npims,pp.gratid,pp.filter, $
						     pp.shuffmod,pp.nasmask,0, $
						     pp.fm4pos,pp.gratpos,pp.campos,pp.focpos, $
						     pp.biasexists,pp.darkexists,pp.flatexists, $
						     pp.skyexists,pp.binning,pp.xbinsize,pp.ybinsize, $
						     pp.ppfname, $
					format = '(a-16,1x,i3,1x,a4,1x,a4,1x,3i1,1x,4i7,1x,4i1,1x,3i1,1x,a)')
						;
						; increment number of ppar files for this object
						pnum = pnum + 1
						uocgrps = [ uocgrps, grp ]
						uocpars = [ uocpars, pp ]
					endif
				endif	; are we done?
			endfor	; loop over this object's observations
		endif else print,pre+': Error - no observations found for: ', sobs[i]
	endfor	; loop over unique object names
	;
	; trim unique object/configuration (uoc) lists
	uocgrps = uocgrps[1:*]
	uocpars = uocpars[1:*]
	nuocg = n_elements(uocgrps)
	print,'Found '+strn(nuocg)+' unique object/configuration groups:'
	print,'SET  OBJECT           NIM  GRT  FLT NSS     FM4   GRAT    CAM    FOC  CAL BIN PPFNAME'
	print,'---  ---------------- --- ---- ---- --- ------- ------ ------ ------ ---- --- -------'
	forprint,indgen(nuocg),uocgrps,form='(i03,2x,a)'
	;
	; loop and get resp and flux cal associations
	;print,'Associate response and flux cal observations to targets'
	;print,'Enter SET triples: object response flux, <cr> to quit'
	;rec = 'start'
	;while strlen(rec) gt 0 do begin
	;	read,'N N N: ',rec
	;	if strlen(rec) gt 0 then begin
	;		io = fix(gettok(rec,' '))
	;		ir = fix(gettok(rec,' '))
	;		ic = fix(rec)
	;		uocpars[io].resp_ppf = uocpars[ir].ppfname
	;		uocpars[io].fcal_ppf = uocpars[ic].ppfname
	;	endif
	;endwhile
	for i=0,nuocg-1 do $
		pcwi_write_ppar,uocpars[i]
	;
	return
end
