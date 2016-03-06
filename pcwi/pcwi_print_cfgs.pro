; $Id: pcwi_print_cfgs.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_PRINT_CFGS
;
; PURPOSE:
;	This function prints a summary of the configurations passed, one
;	line per image.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_PRINT_CFGS,Pcfg
;
; INPUTS:
;	Pcfg	- An array of struct PCWI_CFG.
;
; OUTPUTS:
;	imsum	- image summary (string)
;
; KEYWORDS:
;	header	- set to print headings for the columns
;	silent	- just return string, do not print
;	outfile	- filename to print to
;
; PROCEDURE:
;	Prints a summary allowing comparison of configurations of each image.
;
; EXAMPLE:
;	Read in the stage one processed image data headers in directory 
;	'redux' and return an array of struct PCWI_CFG.  Find all the
;	continuum flats and print their configuration summary.
;
;	KCFG = PCWI_PRINT_CFGS('redux',filespec='*_int.fits')
;	PCWI_PRINT_CFG, KCFG
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-08	Initial version
;	2013-NOV-13	Added outfile keyword
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_print_cfgs,pcfg,imsum,header=header,silent=silent,outfile=outfile
	;
	; setup
	pre = 'PCWI_PRINT_CFGS'
	imsum = ''
	;
	; check inputs
	if pcwi_verify_cfg(pcfg) ne 0 then return
	;
	; outfile?
	if keyword_set(outfile) then begin
		filestamp,outfile,/arch
		openw,ol,outfile,/get_lun
		printf,ol,'# '+pre+'  '+systime(0)
		printf,ol,'# SSM = Sky, Shuffle, Mask: 0 - no, 1 - yes'
		printf,ol,'#  #/   N Bin AMPS SSM GRAT FILT   FM4pos    GRpos   CAMpos   FOCpos   Cwave JDobs         Expt Type     Imno   RA          Dec             PA    Object'
	endif
	;
	; header?
	if keyword_set(header) and not keyword_set(silent) then begin
		print,' SSM = Sky, Shuffle, Mask: 0 - no, 1 - yes'
		print,'   #/   N Bin AMPS SSM GRAT FILT   FM4pos    GRpos   CAMpos   FOCpos   Cwave JDobs         Expt Type     Imno   RA          Dec             PA    Object'
	endif
	;
	; current date
	cdate = 0.d0
	;
	; loop over elements
	n = n_elements(pcfg)
	for i=0,n-1l do begin
		;
		; prepare summary
		imsum = string(i+1,'/',n,pcfg[i].xbinsize,pcfg[i].ybinsize, $
			strtrim(pcfg[i].ampmode,2),pcfg[i].skyobs, $
			pcfg[i].shuffmod,pcfg[i].nasmask, $
			strtrim(pcfg[i].gratid,2),strtrim(pcfg[i].filter,2), $
			pcfg[i].fm4pos,pcfg[i].gratpos,pcfg[i].campos, $
			pcfg[i].focpos,pcfg[i].cwave,pcfg[i].juliandate, $
			pcfg[i].exptime,strtrim(pcfg[i].imgtype,2), $
			pcfg[i].imgnum,pcfg[i].ra,pcfg[i].dec,pcfg[i].rotpa, $
			format='(i4,a1,i4,2i2,1x,a-5,3i1,1x,a-4,1x,a-4,4i9,f8.1,f12.3,f7.1,1x,a-8,i5,2f13.8,2x,f7.2)')
		;
		; add object info
		if strpos(pcfg[i].imgtype,'object') ge 0 then begin
			imsum = imsum + string(strtrim(pcfg[i].object,2),form='(2x,a)')
		endif
		if not keyword_set(silent) then print,imsum
		if keyword_set(outfile) then begin
			if i gt 0 then $
				deljd = pcfg[i].juliandate - pcfg[i-1].juliandate $
			else	deljd = 1.0
			if deljd gt 0.25 and pcfg[i].juliandate-cdate gt 0.75 then begin
				cdate = pcfg[i].juliandate
				caldat,long(cdate),month,day,year
				printf,ol,'# Run: ',year-2000.,month,day, $
					format='(a,i02,i02,i02)'
			endif
			printf,ol,imsum
		endif
	endfor
	if keyword_set(outfile) then free_lun,ol
	;
	return
end
