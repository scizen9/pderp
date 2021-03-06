; $Id: pcwi_geom__define.pro | Tue Apr 21 10:32:09 2015 -0700 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_GEOM__DEFINE
;
; PURPOSE:
;	This procedure defines the structure for all the information for 
;	PCWI geometric transformations.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	pgeom = {PCWI_GEOM}
;
; INPUTS:
;	None
;
; OUTPUTS:
;	None
;
; SIDE EFFECTS:
;	defines the PCWI_GEOM data structure
;
; PROCEDURE:
;	Provide the automatic structure definition for the PCWI geometric
;	transformation structure PCWI_GEOM.
;
; EXAMPLE:
;	Instantiate and initialize a single geom struct for PCWI:
;
;	pgeom = {pcwi_geom}
;	pgeom = struct_init(pgeom)
;
; NOTES:
;	Keep structure name on a line by itself (see struct_init.pro).
;	Keep tag names 15 chars or less.
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-MAY-03	Initial version
;	2013-AUG-12	Added padlo, padhi tags
;	2016-MAR-03	Split from kderp to pderp
;	2016-APR-21	Added rotoff tag
;-
pro pcwi_geom__define
;
tmp = { pcwi_geom, $
;
; output filename
	geomfile:'', $		; output save file for geom struct
;
; calibration images
	cbarsimgnum:0l, $	; cbars image number (0 - 9999)
	cbarsjd:0.d0, $		; cbars image julian date
	cbarsfname:'', $	; cbars image file name
	arcimgnum:0l, $		; arc image number
	arcjd:0.d0, $		; arc image julian date
	arcfname:'', $		; arc image file name
;
; reference spectra
	refname:'', $		; name of reference spectrum ('ThAr', e.g.)
	refspec:'', $		; reference spectrum file
	reflist:'', $		; reference line list
;
; nod and shuffle?
	nasmask:0, $		; 0 - no, 1 - yes
;
; grating properties
	gratid:'', $		; grating id
	gratnum:-1, $		; grating number
	rho:3.0d, $		; grating lines/mm
	slant:0.0d, $		; off-Bragg angle in degrees
	lastdegree:4, $		; highest order for full-ccd wavelength fit
;
; filter properties
	filter:'', $		; filter id
	filtnum:-1, $		; filter number
;
; encoder positions
	campos:0L, $		; Camera encoder position
	gratpos:0L, $		; grating encoder position
	gratanom:0., $		; grating angle anomoly (degrees)
;
; pixel scale
	pxscl:0.00008096d0, $	; degrees per unbinned spatial pixel
;
; slice scale
	slscl:0.00073556d0, $	; degrees per slice
;
; IFU rotation offset
	rotoff:1.58, $		; degrees from rotator PA
;
; CCD binning
	xbinsize:1, $		; binning in x (pixels)
	ybinsize:1, $		; binning in y (pixels)
;
; CCD size
	nx:0, $			; number of x pixels in image
	ny:0, $			; number of y pixels in image
;
; CCD readnoise
	rdnoise:3., $		; e-/pixel
;
; Y ranges
	goody0:0, $		; lowest good y pixel in spectrum
	goody1:0, $		; highest good y pixel in spectrum
	trimy0:0, $		; lowest y pixel to include in output
	trimy1:0, $		; highest y pixel to include in output
;
; cross correlation parameters
	ccwn:100., $		; cross correlation window for bar-to-bar offset
	ccoff:fltarr(24), $	; known offsets relative to reference slice in pixels
;
; wavelengths
	cwave:-9., $		; central wavelength (Angstroms)
	wave0out:-9., $		; output wavelength zeropoint
	wave1out:-9., $		; output ending wavelength
	dwout:-9., $		; output dispersion (Ang/pix)
	wavran:-9., $		; approximate wavelength range (Ang)
	halfwidth:3, $		; instrumental half-width of resolution (Ang)
	resolution:1., $	; gaussian sigma of instrumental resol. (Ang)
	pxwindow:0, $		; window size for finding lines in pixels
	waveall0:-9., $		; low wavelength that includes all data
	waveall1:-9., $		; high wavelength that includes all data
	wavegood0:-9., $	; low wavelength that includes all good data
	wavegood1:-9., $	; high wavelength that includes all good data
	wavemid:-9., $		; wavelength in the middle of the ranges
	wavefid:3000., $	; fiducial wavelength for aligning all solns.
;
; control points
	xi:fltarr(6120), $	; x input
	yi:fltarr(6120), $	; y input
	xo:fltarr(6120), $	; x output
	yo:fltarr(6120), $	; y output
	xw:fltarr(6120), $	; x output in wavelength space
	yw:fltarr(6120), $	; y output in wavelength space
	bar:intarr(6120), $	; bar number for each point
	slice:intarr(6120), $	; slice number for each point
;
; Reference bar for wavelength solution
	refbar:57, $		; 0 - 119 (defaults to 57)
	rbcoeffs:fltarr(9), $	; polynomial wavelength solution up to 9 coeffs
	refoutx:fltarr(5), $	; ref output x control point positions (pixels)
	refdelx:-9., $		; ref delta x (pixels)
;
; X values for bars
	barx:fltarr(120), $	; measured in cbars image
	refx:fltarr(120), $	; reference x for extracted arc spectra
	x0out:30, $		; output spatial zeropoint (unbinned pix)
;
; Y offset for each bar
	baroff:fltarr(120), $	; pixel offset relative to refbar
;
; Wavelength fit for each bar
	bfitcoeffs:fltarr(9,120),$ ; polynomial wavelength solution
	bfitord:6, $		; order of polynomial wavelength solution
;
; Which image row was used for canonical x position?
	midrow:0L, $		; image pixel
;
; Coefficients
	kx:dblarr(4,4), $	; simple de-warping
	ky:dblarr(4,4), $	;
	kwx:dblarr(6,6,24), $	; wavelength solution included for each slice
	kwy:dblarr(6,6,24), $	;
	ypad:600, $		; number of pixels to pad image in y
;
; Status
	xrsd:fltarr(24), $	; transform x residuals (pixels)
	yrsd:fltarr(24), $	; transform y residuals (pixels)
	avewavesig:0., $	; average bar wavelength sigma in Angstroms
	stdevwavesig:0., $	; standard deviation of bar sigmas in Angstroms
	initialized:0, $	; 0 - no, 1 - yes
	status:-1, $		; 0 - good fit, else not good
	progid:'', $		; program that last modified the geom file
;
; timestamp
	timestamp:0.d0 $	; timestamp for struct (unix seconds)
	}
end
