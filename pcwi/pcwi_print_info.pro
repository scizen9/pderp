; $Id: pcwi_print_info.pro | Tue Mar 3 16:16:17 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_PRINT_INFO
;
; PURPOSE:
;	This function prints info to the screen and to logfile
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_PRINT_INFO,Ppar,Pre,Msg,Var1,Var2...
;
; INPUTS:
;	Ppar	- array of struct PCWI_PPAR pipeline parameters
;	Pre	- a prefix string containing the name of the calling procedure
;	Msg	- the message to print
;	Var1	- the variables to print
;
; KEYWORDS:
;	ERROR	- Set if message is an error message
;	WARNING	- Set if message is a warning message
;	INFO	- Set if message is only for information, can also set to
;		  level: will output if ppar.verbose ge level
;	FORMAT	- Set to format string for variables
;
; PROCEDURE:
;	Constructs an output string using passed variables and format
;	string.  All warnings and errors are printed.  Info messages are
;	not printed unless their priority is less than or equal to the
;	verbose level in ppar.verbose.  Thus, if ppar.verbose = 0, a
;	standard info message is not printed.  By raising the priority
;	rank, this requires the verbosity to be higher before it is printed.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-SEP-16	Initial version
;	2013-NOV-21	Refinement of print rank logic
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_print_info,ppar,ipre,msg,v1,v2,v3,v4,v5,v6,v7,v8,v9, $
	error=error, warning=warning, info=info, format=format
	;
	; check inputs
	; ppar struct
	if pcwi_verify_ppar(ppar) ne 0 then $
		ppar = { pcwi_ppar }
	; pre string
	if size(ipre,/type) ne 7 then begin
		print,'PCWI_PRINT_INFO: Error - input pre string error, returning'
		return
	endif
	;
	; make pre string standard length (20)
	pre = ipre
	while strlen(pre) lt 20 do $
		pre = pre + ' '
	;
	; check keywords
	if keyword_set(info) then $
		prank = info $
	else	prank = 1	; default rank
	;
	; default label (info) plus rank
	ostr = pre+' [INFO '+strtrim(string(prank),2)+' ] '+msg
	if keyword_set(warning) then begin
		prank = 0
		ostr = pre+' [WARNING] '+msg
	endif
	if keyword_set(error) then begin
		prank = 0
		ostr = pre+' [ERROR  ] '+msg
	endif
	;
	; check input variables
	np = n_params(0) - 3
	if np gt 0 then begin
		ostr = ostr + ': '
		case np of
			1: ostr = string(ostr,v1,format=format)
			2: ostr = string(ostr,v1,v2,format=format)
			3: ostr = string(ostr,v1,v2,v3,format=format)
			4: ostr = string(ostr,v1,v2,v3,v4,format=format)
			5: ostr = string(ostr,v1,v2,v3,v4,v5,format=format)
			6: ostr = string(ostr,v1,v2,v3,v4,v5,v6,format=format)
			7: ostr = string(ostr,v1,v2,v3,v4,v5,v6,v7,format=format)
			8: ostr = string(ostr,v1,v2,v3,v4,v5,v6,v7,v8,format=format)
			9: ostr = string(ostr,v1,v2,v3,v4,v5,v6,v7,v8,v9,format=format)
			else: print,'PCWI_PRINT_INFO [ERROR] variable overflow'
		endcase
	endif
	;
	; check logging first
	if ppar.loglun gt 0 then begin
		printf,ppar.loglun,ostr
		flush,ppar.loglun
	endif
	;
	; check verbosity
	if prank le ppar.verbose then $
		print,ostr
	;
	return
end
