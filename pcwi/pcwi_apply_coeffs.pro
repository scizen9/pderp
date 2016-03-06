; $Id: pcwi_apply_coeffs.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_APPLY_COEFFS
;
; PURPOSE:
;	Applies wavelength solution coefficients to control points
;	for the given arc bar spectrum.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_APPLY_COEFFS,Pgeom,Barno,Coeffs
;
; INPUTS:
;	Pgeom	- PCWI_GEOM struct from PCWI_TRACE_CBARS and PCWI_EXTRACT_ARCS
;	Barno	- which bar spectrum to apply solution to
;
; INPUT KEYWORDS:
;	VERBOSE - extra output
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Updates Pgeom XW and YW tags to have appropriate values for input
;	wavelength solution coefficients.
;
; PROCEDURE:
;	Uses reference bar solution to define wavelength zeropoint and
;	dispersion.  Applies wavelength solution to original output control
;	points (Pgeom.[xo,yo]) to derive real wavelength coordinates of
;	each, then subtracts the wavelength zeropoint and divides by the
;	reference dispersion to generate psuedo wavelength pixel coordinates.
;
; EXAMPLE:
;	Define the geometry from a 'cbars' image and use it to extract and 
;	display the spectra from an 'arc' image from the same calibration
;	sequence.
;
;	cbars = mrdfits('image7142_int.fits',0,chdr)
;	pcwi_trace_cbars,cbars,Pgeom,/centroid
;	arc = mrdfits('image7140_int.fits',0,ahdr)
;	pcwi_extract_arcs,arc,pgeom,arcspec,/verbose
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-JUL-31	Initial Revision
;	2013-SEP-26	Uses reference slice for output control points
;	2016-MAR-03	Split from kderp to pderp
;-
;
pro pcwi_apply_coeffs,pgeom,barno,coeffs, $
	verbose=verbose, help=help
;
; startup
pre = 'PCWI_APPLY_COEFFS'
q = ''
;
; check inputs
if n_params(0) lt 3 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', Pgeom, Barno, Coeffs'
	return
endif
;
; Check Pgeom
ksz = size(pgeom)
if ksz[2] eq 8 then begin
	if pgeom.initialized ne 1 then begin
		print,pre+': Error - Pgeom struct not initialized.'
		return
	endif
endif else begin
	print,pre+': Error - Pgeom not legal, run PCWI_TRACE_CBARS and PCWI_EXTRACT ARCS first.'
	return
endelse
;
; check reference solution
if total(pgeom.rbcoeffs) eq 0. or pgeom.rbcoeffs[0] eq 0. or $
	 pgeom.rbcoeffs[1] eq 0. then begin
	print,pre+': Error - Pgeom reference bar coefficients not set, run PCWI_SOLVE_ARCS first.'
	return
endif
;
; check Barno
if barno lt 0 or barno gt 119 then begin
	print,pre+': Error - Bar number out of range (0-119): ',barno
	return
endif
;
; reference bar in same slice as pgeom.refbar
refbar = (barno mod 5)
;
; get control points
t=where(pgeom.bar eq barno and pgeom.xi gt 0.)
;
; spatial axis
; use reference slice, but adjust to left edge
refoutx = pgeom.refoutx - min(pgeom.refoutx) + pgeom.x0out
xo = refoutx[refbar]
;
; wavelength axis
yo = pgeom.yo[t]
;
; get reference wavelength
wave0 = pgeom.wave0out
;
; apply coeffs
xw = xo						; nothing to apply
yw = ( poly(yo,coeffs) - wave0 ) / pgeom.dwout	; apply wave soln.
;
; insert into pgeom
pgeom.xw[t] = xw
pgeom.yw[t] = yw
;
; insert fit coeffs for this bar
fo = pgeom.bfitord
pgeom.bfitcoeffs[0:fo,barno] = coeffs[0:fo]
;
; Pgeom timestamp
pgeom.timestamp = systime(1)
;
return
end
