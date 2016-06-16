; $Id: pcwi_read_atlas.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_READ_ATLAS
;
; PURPOSE:
;	Read the atlas spectrum and convolve to nominal PCWI resolution
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_READ_ATLAS, Pgeom, Ppar, Refspec, Refwave, Refdisp
;
; INPUTS:
;	Pgeom	- PCWI_GEOM struct from PCWI_TRACE_CBARS and PCWI_EXTRACT_ARCS
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Refspec	- Atlas reference spectrum
;	Refwave	- Atlas reference spectrum wavelengths
;	Refdisp	- Atlas reference spectrum dispersion in Ang/px
;
; INPUT KEYWORDS:
;
; SIDE EFFECTS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill
;	2014-SEP-18	Initial Revision
;	2016-MAR-03	Split from kderp to pderp
;-
;
pro pcwi_read_atlas, pgeom, ppar, refspec, refwave, refdisp

pre = 'PCWI_READ_ATLAS'
q=''
;
; init
refspec = -1.
refwave = -1.
refdisp = -1.
;
; check inputs
if pcwi_verify_geom(pgeom,/init) ne 0 then return
if pcwi_verify_ppar(ppar,/init) ne 0 then return
;
; canonical resolution?
resolution = pgeom.resolution
;
; check if file is available
if not file_test(pgeom.refspec,/read,/regular) then begin
	pcwi_print_info,ppar,pre,'Atlas spectrum file not found',pgeom.refspec,$
		format='(a,a)',/error
	return
endif
;
; report the read
pcwi_print_info,ppar,pre,'Reading atlas spectrum in',pgeom.refspec, $
	format='(a,1x,a)'
;
; load the reference atlas spectrum.
rdfits1dspec,pgeom.refspec,refwave,atlas, $
	wavezero=refw0, deltawave=refdisp, refpix=refpix
refspec = atlas>0  
;
; we want to degrade this spectrum to the instrument resolution
xx = findgen(99)-50.0d
fwhm = resolution/refdisp
gaus = gaussian(xx,[1.0,0.0,fwhm/2.355])
gaus /= total(gaus)
refspec = convolve(refspec,gaus)
;
return
end		; pcwi_read_atlas
