; $Id: pcwi_verify_dirs.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_VERIFY_DIRS
;
; PURPOSE:
;	This function verifies the input PCWI_PPAR struct.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	Result = PCWI_VERIFY_DIRS(Ppar,Rawdir,Reddir,Cdir,Ddir)
;
; INPUTS:
;	Ppar	- array of struct PCWI_PPAR
;
; RETURNS:
;	-1 - error
;	 0 - all dirs exist and accessible
;	 1 - raw dir not accessible
;	 2 - reduced dir not accessible
;	 3 - calib dir not accessible
;	 4 - data dir not accessible
;
; OPTIONAL OUTPUTS:
;	Rawdir	- raw image directory
;	Reddir	- reduced image directory
;	Cdir	- calib directory
;	Ddir	- data directory
;
; KEYWORDS:
;	NOCREATE - set to prevent creation of output directory
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-16	Initial version
;	2014-APR-03	Added calib dir check
;	2015-FEB-20	Renamed in/out dirs to raw/red
;	2016-MAR-03	Split from kderp to pderp
;-
function pcwi_verify_dirs,ppar,rawdir,reddir,cdir,ddir,nocreate=nocreate
	;
	; setup
	pre = 'PCWI_VERIFY_DIRS'
	q = ''
	;
	; check input
	if pcwi_verify_ppar(ppar,/init) ne 0 then return,-1
	;
	; directories
	rawdir = pcwi_expand_dir(ppar.rawdir)
	reddir = pcwi_expand_dir(ppar.reddir)
	cdir = pcwi_expand_dir(ppar.caldir)
	ddir = pcwi_expand_dir(ppar.datdir)
	;
	; check if rawdir exists and is readable
	if not file_test(rawdir,/directory,/executable,/read) then begin
		pcwi_print_info,ppar,pre,'cannot access raw image dir',rawdir,/error
		return,1
	endif
	;
	; check if reddir exists
	if not file_test(reddir,/directory) then begin
		if not keyword_set(nocreate) then begin
			print,pre+': Warning - reduced image dir does not exist: ',reddir
			read,'Create? (Y/n): ',q
			q = strupcase(strtrim(q,2))
			if strmid(q,0,1) ne 'N' then begin
				file_mkdir,reddir,/noexpand
				pcwi_print_info,ppar,pre,'created directory',reddir
			endif 
		endif else begin
			pcwi_print_info,ppar,pre,'no reduced image dir',/error
			return,2
		endelse
	endif
	;
	; check if reddir accessible
	if not file_test(reddir,/directory,/executable,/write) then begin
		pcwi_print_info,ppar,pre, 'reduced image dir not accessible',/error
		return,2
	endif
	;
	; check if cdir accessible
	if not file_test(cdir,/directory,/executable,/read,/write) then begin
		pcwi_print_info,ppar,pre, 'calib dir not accessible',/error
		return,3
	endif
	;
	; check if ddir exists and is readable
	if not file_test(ddir,/directory,/executable,/read) then begin
		pcwi_print_info,ppar,pre,'cannot access data dir',ddir,/error
		return,4
	endif
	;
	; if we get here all is well
	return,0
end
