; $Id: pcwi_solve_geom.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_SOLVE_GEOM
;
; PURPOSE:
;	Solve the wavelength solutions for each arc spectrum
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_SOLVE_GEOM,Spec, Pgeom, Ppar
;
; INPUTS:
;	Spec	- a array of arc spectra produced by PCWI_EXTRACT_ARCS
;	Pgeom	- PCWI_GEOM struct from PCWI_TRACE_CBARS and PCWI_EXTRACT_ARCS
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; INPUT KEYWORDS:
;
; SIDE EFFECTS:
;	Modifies PCWI_GEOM struct by calculating new control points that
;	take into account the wavelength solution.
;	NOTE: sets KGEOM.STATUS to 0 if fitting succeeded, otherwise sets to
;	1 or greater depending on reason for failure (see pcwi_solve_arcs.pro).
;
; PROCEDURE:
;	Find the wavelength solution of the reference bar arc and then
;	propogate it to the other bars.  Record the wavelength solution
;	in the wavelength control points in Pgeom.
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
;	pcwi_solve_geom,arcspec,pgeom,ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-SEP-18	Initial Revision
;	2016-MAR-03	Split from kderp to pderp
;-
;
pro pcwi_solve_geom,spec,pgeom,ppar, fitdisp=fitdisp, help=help
;
; startup
pre = 'PCWI_SOLVE_GEOM'
q = ''
;
; check inputs
if n_params(0) lt 2 or keyword_set(help) then begin
	print,pre+': Info - Usage: '+pre+', ArcSpec, Pgeom'
	return
endif
;
; Check structs
if pcwi_verify_geom(pgeom,/init) ne 0 then return
if pcwi_verify_ppar(ppar,/init) ne 0 then return
;
; check spec
ssz = size(spec)
if ssz[0] ne 2 or ssz[2] ne 120 then begin
	pcwi_print_info,ppar,pre,'Input spec array malformed, run PCWI_EXTRACT_ARCS first.',/error
	return
endif
;
; plot file
p_fmt = '(i0'+strn(ppar.fdigits)+')'
plfil = ppar.reddir+'wave_cb' + string(pgeom.cbarsimgnum,p_fmt) + $
		       '_arc' + string(pgeom.arcimgnum,p_fmt)
;
; solve arc spectra
pcwi_solve_arcs,spec,pgeom,ppar,/tweak,plot_file=plfil
;
; solve transformation on slice-by-slice basis
pcwi_solve_slices,ppar,pgeom
;
return
end
