; $Id: pcwi_read_cfgs.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_READ_CFGS
;
; PURPOSE:
;	This function reads all the fits headers in the specified directory and
;	populates an array of PCWI_CFG structs, one for each file.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_READ_CFGS( InputDir )
;
; OPTIONAL INPUTS:
;	InputDir	- input directory (string) defaults to current dir
;
; INPUT KEYWORDS:
;	FILESPEC- set to use a specific file spec (def: 'image*.fit*')
;	SILENT	- set to silence output
;
; OUTPUT KEYWORDS:
;	COUNT	- contains the number of images found
;
; RETURNS:
;	An array of struct PCWI_CFG, with one element for each FITS header
;	in InputDir.
;
; PROCEDURE:
;	Analyzes the FITS headers of the images in InputDir and then creates
;	an array of the PCWI_CFG struct, setting the tags for each entry
;	based on header keywords.  Sorts entries based on time.
;
; EXAMPLE:
;	Read in the stage one processed image data headers in directory 
;	'redux' and return an array of struct PCWI_CFG that can be used 
;	to group or compare observations.
;
;	KCFG = PCWI_READ_CFGS('redux',filespec='*_int.fits')
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-03	Initial version
;	2013-MAY-11	Added FILESPEC keyword
;	2013-MAY-14	Made default filespec 'image????.fit*'
;	2013-AUG-02	added STAGE1 keyword
;	2013-SEP-05	added STAGE2 and STAGE3 keywords
;	2014-APR-03	added count,silent output keywords
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_read_cfgs,inputdir, $
	filespec=filespec, count=count, silent=silent
	;
	; setup
	pre = 'PCWI_READ_CFGS'
	;
	; check inputs
	if n_elements(inputdir) le 0 then inputdir = './'
	;
	; expand path
	indir = pcwi_expand_dir(inputdir)
	;
	; check keywords
	if keyword_set(filespec) then $
		fspec = filespec $
	else	fspec = '*.fit*'
	;
	; get a blank copy of the PCWI_CFG struct
	A = {pcwi_cfg}
	A = struct_init(A)
	;
	; get file list
	flist = file_search(indir+fspec,count=count)
	if count le 0 then begin
		if not keyword_set(silent) then $
			print,pre+': Error - no files found: ',indir+fspec
		return,A
	endif
	;
	; set up array
	pcfg = replicate(A,count)
	;
	; populate them
	for i=0,count-1 do $
		pcfg[i] = pcwi_read_cfg(flist[i])
	;
	; sort on time
	pcfg = pcfg[sort(pcfg.juliandate)]
	;
	return,pcfg
end
