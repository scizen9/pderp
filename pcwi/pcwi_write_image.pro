; $Id: pcwi_write_image.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_WRITE_IMAGE
;
; PURPOSE:
;	Write input 2-d or 3-d image to disk.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_WRITE_IMAGE, Img, Hdr, FileName, Ppar
;
; INPUTS:
;	Img	- 2-d or 3-d image
;	Hdr	- FITS header for image
;	FileName- filename for output used with Ppar.reddir to specify full
;		output path
;	Ppar	- PCWI_PPAR struct specifying outputs and logging flags
;
; KEYWORDS:
;	LOGLUN	- the logfile logical unit
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	Outputs image files in output directory specified by the
;	PCWI_PPAR struct Ppar.  Updates logfile if LOGLUN specified
;	and outputs to screen if Ppar.verbose is set to 1.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-AUG-17	Initial version
;	2013-SEP-11	Now use ppar.loglun for logging
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_write_image,img,hdr,filename,ppar,iscale=iscale,help=help
	;
	; setup
	pre = 'PCWI_WRITE_IMAGE'
	;
	; help request
	if keyword_set(help) then begin
		print,pre+': Info - Usage: '+pre+', Img, Hdr, FileName, Ppar'
		print,pre+': Info - Keywords: LOGLUN=<logfile_lunit>, /HELP'
		return
	endif
	;
	; check ppar struct
	if pcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; check inputs
	odir = pcwi_expand_dir(ppar.reddir)
	if not file_test(odir,/directory,/executable,/write) then begin
		print,pre+': Error - Output directory not accessible: ',odir
		return
	endif
	;
	; get output filename
	ofil = odir + filename
	;
	; check if it exists
	if file_test(ofil) then begin
		;
		; clobber it, if requested
		if ppar.clobber eq 1 then begin
			file_delete,ofil,verbose=ppar.verbose
			pcwi_print_info,ppar,pre,'deleted existing file',ofil,format='(a,a)'
		;
		; or skip writing and report
		endif else begin
			pcwi_print_info,ppar,pre,'existing file undisturbed',ofil,format='(a,a)'
			return
		endelse
	endif
	;
	; time stamp
	get_date,dstr,/time
	sxaddpar,hdr,'KDRPDATE',dstr,' PCWI DRP image write date'
	;
	; write it out
	mwrfits,img,ofil,hdr,iscale=iscale
	;
	; log
	pcwi_print_info,ppar,pre,'wrote image file',ofil,format='(a,a)'
	;
	return
end
