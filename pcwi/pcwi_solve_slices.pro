; $Id: pcwi_solve_slices.pro | Fri Apr 24 12:15:57 2015 -0700 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_SOLVE_SLICES
;
; PURPOSE:
;	Solves the geometric transformation for each slice.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_SOLVE_SLICES,Ppar,Pgeom
;
; INPUTS:
;	Pgeom	- PCWI_GEOM struct from PCWI_TRACE_CBARS and PCWI_EXTRACT_ARCS
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;	HELP	- display usage help and exit
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Updates Pgeom KWX and KWY geomtric transformation coefficients.
;
; PROCEDURE:
;	The Pgeom KWX and KWY geomtric transformation coefficients are fit
;	with POLYWARP based on control points in Pgeom XI,YI,XW,YW for 
;	each slice.
;
; EXAMPLE:
;	Define the geometry from a 'cbars' image and use it to extract and 
;	display the spectra from an 'arc' image from the same calibration
;	sequence.
;
;	cbars = mrdfits('image7142_int.fits',0,chdr)
;	pcwi_trace_cbars,cbars,Pgeom,/centroid
;	arc = mrdfits('image7140_int.fits',0,ahdr)
;	pcwi_extract_arcs,arc,pgeom,arcspec
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-31	Initial Revision
;	2013-AUG-12	Added low pixel end padding
;	2014-SEP-11	Now pass ppar
;	2016-MAR-03	Split from kderp to pderp
;-
;
pro pcwi_solve_slices,ppar,pgeom, help=help
;
; setup
pre = 'PCWI_SOLVE_SLICES'
q = ''
;
; check inputs
if n_params(0) lt 1 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Ppar, Pgeom'
	return
endif
;
; Check structs
if pcwi_verify_geom(pgeom,/init) ne 0 then return
if pcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check fit status
if pgeom.status ne 0 then begin
	pcwi_print_info,ppar,pre,'Pgeom fit no good.',/error
	return
endif
;
; diagnostic plots
display = (ppar.display ge 4)
if display then begin
	window,0,title='pcwi_solve_slices'
	!p.multi=[0,1,2]
endif
;
; degree
if pgeom.nasmask eq 1 then $
	degree = 3 $
else	degree = 3
;
; loop over slices
for i=0,23 do begin
	sli = where(pgeom.slice eq i and pgeom.xi gt 0. and $
		    finite(pgeom.xw) and finite(pgeom.yw), nsli)
	if nsli le 0 then begin
		pcwi_print_info,ppar,pre,'Pgeom slice index error.',/error
		return
	endif
	;
	; get control points
	xi = pgeom.xi[sli]
	yi = pgeom.yi[sli] + pgeom.ypad	; pad to avoid data cutoff
	xw = pgeom.xw[sli]
	yw = pgeom.yw[sli]
	;
	; fit
	polywarp,xi,yi,xw,yw,degree,kwx,kwy,/double,status=status
	;
	; get residuals
	pcwi_geom_resid,xi,yi,xw,yw,degree,kwx,kwy,xrsd,yrsd
	;
	; check status
	if status ne 0 then $
		pcwi_print_info,ppar,pre,'Polywarp non-zero status: ',status, $
			/warning
	;
	; insert into pgeom
	pgeom.kwx[0:degree,0:degree,i] = kwx
	pgeom.kwy[0:degree,0:degree,i] = kwy
	;
	; insert residuals
	xmo = moment(xrsd,/nan)
	ymo = moment(yrsd,/nan)
	pgeom.xrsd[i] = sqrt(xmo[1])
	pgeom.yrsd[i] = sqrt(ymo[1])
	;
	; plot if requested
	if display then begin
		tlab = 'Slice '+strn(i) + ':' + $
			' Xsig = '+string(pgeom.xrsd[i], format='(f7.3)') + $
			' Ysig = '+string(pgeom.yrsd[i], format='(f7.3)')
		;
		; x residuals
		xrng=get_plotlims(xw)
		yrng = [min([min(xrsd),-0.2]),max([max(xrsd),0.2])]
		plot,xw,xrsd,psym=4,title=tlab, $
			xran=xrng,/xs,xtitle='X coord (pix)', $
			yran=yrng,/ys,ytitle='X rsd (pix)'
		oplot,!x.crange,[0,0],linesty=0
		oplot,!x.crange,[xmo[0],xmo[0]],linesty=2
		oplot,!x.crange,[xmo[0]+pgeom.xrsd[i],xmo[0]+pgeom.xrsd[i]],linesty=2
		oplot,!x.crange,[xmo[0]-pgeom.xrsd[i],xmo[0]-pgeom.xrsd[i]],linesty=2
		;
		; y residuals
		yw = yw*pgeom.dwout + pgeom.wave0out
		xrng=get_plotlims(yw)
		yrng = [min([min(yrsd),-0.2]),max([max(yrsd),0.2])]
		plot,yw,yrsd,psym=4, $
			xran=xrng,/xs,xtitle='Y coord (Ang)', $
			yran=yrng,/ys,ytitle='Y rsd (pix)'
		oplot,!x.crange,[0,0],linesty=0
		oplot,!x.crange,[ymo[0],ymo[0]],linesty=2
		oplot,!x.crange,[ymo[0]+pgeom.yrsd[i],ymo[0]+pgeom.yrsd[i]],linesty=2
		oplot,!x.crange,[ymo[0]-pgeom.yrsd[i],ymo[0]-pgeom.yrsd[i]],linesty=2
		read,'Next? (Q - quit plotting, <cr> - next): ',q
		if strupcase(strmid(q,0,1)) eq 'Q' then display = (1 eq 0)
	endif	; display
endfor	; loop over slices
;
; Pgeom timestamp
pgeom.progid = pre
pgeom.timestamp = systime(1)
;
if ppar.display ge 4 then $
	!p.multi=0
;
return
end
