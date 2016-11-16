; $Id: pcwi_make_std.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_MAKE_STD
;
; PURPOSE:
;	This procedure creates a standard star inverse sensitivity
;	spectrum (in units of Flam/e-) from the input data cube.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_MAKE_STD, Pcfg,  Ppar, Invsen
;
; INPUTS:
;	Pcfg	- PCWI_CFG struct for the input data cube, preferrably
;			from a sky or dome-flat observation
;	Ppar	- PCWI_PPAR pipeline parameter struct
;
; OUTPUTS:
;	Invsen	- a vector giving the inverse sensitivity in Flam/e-
;
; SIDE EFFECTS:
;	Outputs a fits image of the standard star inverse sensitivity with 
;	same image number root as the input file, but with '_std'
;	appended. For example, if 'image1234.fits' is pointed to by the
;	input PCWI_CFG struct, then the output std image would have the
;	filename 'image1234_std.fits'.
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Find the standard star in the slices, sky subtract and then add up
;	the flux.  Read in standard star flux and divide to get effective
;	inverse sensitivity (Flam/e-).
;
; EXAMPLE:
;
; TODO:
;	fit low-order polynomial to invsen function
;	mask known atmospheric lines/bands
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2014-APR-22	Initial Revision
;	2014-SEP-23	Added extinction correction
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_make_std,pcfg,ppar,invsen
	;
	; setup
	pre = 'PCWI_MAKE_STD'
	q=''
	;
	; check inputs
	if pcwi_verify_cfg(pcfg,/init) ne 0 then return
	if pcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	pcwi_print_info,ppar,pre,systime(0)
	;
	; is this a standard star object observation?
	if strmatch(strtrim(pcfg.imgtype,2),'object') eq 0 then begin
		pcwi_print_info,ppar,pre,'not a std obs',/warning
	endif
	;
	; directories
	if pcwi_verify_dirs(ppar,rawdir,reddir,cdir,ddir) ne 0 then begin
		pcwi_print_info,ppar,pre,'Directory error, returning',/error
		return
	endif
	;
	; get output image (in reduced directory)
	ofil = pcwi_get_imname(ppar,pcfg.imgnum,'_invsens',/reduced)
	if file_test(ofil) then begin
		if ppar.clobber ne 1 then begin
			pcwi_print_info,ppar,pre,'output file already exists',ofil,/error
			return
		endif else $
			pcwi_print_info,ppar,pre,'output file will be overwritten',ofil,/warning
	endif
	;
	; read in image
	icub = pcwi_read_image(pcfg.imgnum,ppar,'_icuber',hdr,/calib,status=stat)
	if stat ne 0 then begin
		pcwi_print_info,ppar,pre,'could not read input file',/error
		return
	endif
	;
	; check standard
	sname = strlowcase(strtrim(sxpar(hdr,'object'),2))
	;
	; is standard file available?
	spath = !PCWI_DATA + '/stds/'+sname+'.fits'
	if not file_test(spath) then begin
		pcwi_print_info,ppar,pre,'standard star data file not found for: '+sname,/error
		return
	endif
	pcwi_print_info,ppar,pre,'generating effective inverse sensitivity curve from '+sname
	;
	; get size
	sz = size(icub,/dim)
	;
	; default pixel ranges
	y = findgen(sz[2])
	y0 = 175
	y1 = sz[2] - 175
	;
	; get exposure time
	expt = sxpar(hdr,'EXPTIME')
	if expt eq 0. then begin
		pcwi_print_info,ppar,pre,'no exposure time found, setting to 1s',/warn
		expt = 1.
	endif else $
		pcwi_print_info,ppar,pre,'Using exposure time of',expt,/info
	;
	; get wavelength scale
	w0 = sxpar(hdr,'CRVAL3')
	dw = sxpar(hdr,'CD3_3')
	crpixw = sxpar(hdr,'CRPIX3')
	;
	; get all good wavelength range
	wgoo0 = sxpar(hdr,'WAVGOOD0')
	wgoo1 = sxpar(hdr,'WAVGOOD1')
	;
	; get all inclusive wavelength range
	wall0 = sxpar(hdr,'WAVALL0')
	wall1 = sxpar(hdr,'WAVALL1')
	;
	; get telescope and atm. correction
	tel = strtrim(sxpar(hdr,'telescop'),2)
	atm = 1./( sxpar(hdr,'avexcor')>1. )
	area = -1.0
	refl = 1.0
	if strpos(tel,'5m') ge 0 then begin
		area = 194165.d0	; Hale 5m area in cm^2
		refl = 0.90		; reflectivity (2-bounce)
	endif
	area = area * refl * atm
	tlab = ' ' + tel + ' * ' + $
		string(refl*100.,form='(i2)')+ '% refl. * ' + $
		string(atm*100.,form='(i2)')+ '% atmos.'
	;
	; compute good y pixel ranges
	if w0 gt 0. and dw gt 0. and wgoo0 gt 0. and wgoo1 gt 0. then begin
		y0 = fix( (wgoo0 - w0) / dw ) + 10
		y1 = fix( (wgoo1 - w0) / dw ) - 10
	endif
	gy = where(y ge y0 and y le y1)
	;
	; wavelength scale
	w = w0 + y*dw
	;
	; good spatial range
	gx0 = ppar.slicex0/pcfg.xbinsize
	gx1 = ppar.slicex1/pcfg.xbinsize
	x = indgen(sz[0])
	;
	;
	; log results
	pcwi_print_info,ppar,pre,'Invsen. Pars: X0, X1, Y0, Y1, Wav0, Wav1', $
		gx0,gx1,y0,y1,w[y0],w[y1],format='(a,4i6,2f9.3)'
	;
	; display status
	doplots = (ppar.display ge 2)
	;
	; find standard in slices
	tot = total(icub[gx0:gx1,*,y0:y1],3)
	xx = findgen(gx1-gx0)+gx0
	mxsl = -1
	mxsg = 0.
	for i=0,23 do begin
		mo = moment(tot[*,i])
		if sqrt(mo[1]) gt mxsg then begin
			mxsg = sqrt(mo[1])
			mxsl = i
		endif
	endfor
	;
	; relevant slices
	sl0 = (mxsl-3)>0
	sl1 = (mxsl+3)<23
	;
	; get x position of std
	cx = (pkfind(tot[*,mxsl],npeaks,thresh=0.99))[0] + gx0
	;
	; log results
	pcwi_print_info,ppar,pre,'Std slices; max, sl0, sl1, spatial cntrd', $
		mxsl,sl0,sl1,cx,format='(a,3i4,f9.2)'
	;
	; do sky subtraction
	scub = icub
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	skywin = ppar.psfwid/pcfg.xbinsize
	for i=sl0,sl1 do begin
		skyspec = fltarr(sz[2])
		for j = 0,sz[2]-1 do begin
			skyv = reform(icub[gx0:gx1,i,j])
			gsky = where(xx le (cx-skywin) or xx ge (cx+skywin))
			sky = median(skyv[gsky])
			skyspec[j] = sky
			scub[*,i,j] = icub[*,i,j] - sky
		endfor
		if doplots then begin
			yrng = get_plotlims(skyspec[gy])
			plot,w,skyspec,title='Slice '+strn(i)+ $
				' SKY, Img #: '+strn(pcfg.imgnum), $
				xtitle='Wave (A)', xran=[wall0,wall1], /xs, $
				ytitle='Sky e-', yran=yrng, /ys
			oplot,[wgoo0,wgoo0],!y.crange,color=colordex('green'), $
				thick=3
			oplot,[wgoo1,wgoo1],!y.crange,color=colordex('green'), $
				thick=3
			read,'Next? (Q-quit plotting, <cr> - next): ',q
			if strupcase(strmid(strtrim(q,2),0,1)) eq 'Q' then $
				doplots = 0
		endif
	endfor
	;
	; recover plot status
	doplots = (ppar.display ge 2)
	;
	; apply extinction correction
	pcwi_correct_extin, scub, hdr, ppar
	;
	; get slice spectra
	slspec = total(scub[gx0:gx1,*,*],1)
	;
	; summed observed standard spectra
	obsspec = total(slspec[sl0:sl1,*],1)
	;
	; convert to e-/second
	obsspec = obsspec / expt
	;
	; read in standard
	sdat = mrdfits(spath,1,shdr)
	swl = sdat.wavelength
	sflx = sdat.flux	; Units of Flam
	sfw = sdat.fwhm
	;
	; get region of interest
	sroi = where(swl ge wall0 and swl le wall1, nsroi)
	if nsroi le 0 then begin
		pcwi_print_info,ppar,pre,'no standard wavelengths in common',/error
		return
	;
	; very sparsely sampled w.r.t. object
	endif else if nsroi eq 1 then begin
		;
		; up against an edge, no good
		if sroi[0] le 0 or sroi[0] ge n_elements(swl)-1L then begin
			pcwi_print_info,ppar,pre, $
				'standard wavelengths not a good match',/error
			return
		;
		; manually expand sroi to allow linterp to work
		endif else begin
			sroi = [ sroi[0]-1, sroi[0], sroi[0]+1 ]
		endelse
	endif
	swl = swl[sroi]
	sflx = sflx[sroi]
	sfw = sfw[sroi]
	fwhm = max(sfw)
	pcwi_print_info,ppar,pre,'reference spectrum FWHM used',fwhm,'Angstroms', $
		format='(a,f5.1,1x,a)'
	;
	; smooth to this resolution
	if pcfg.nasmask then $
		obsspec = gaussfold(w,obsspec,fwhm) $
	else	obsspec = gaussfold(w,obsspec,fwhm,lammin=wgoo0,lammax=wgoo1)
	;
	; resample standard onto our wavelength grid
	linterp,swl,sflx,w,rsflx
	;
	; get effective inverse sensitivity
	invsen = rsflx / obsspec
	;
	; convert to photons
	rspho = 5.03411250d+07 * rsflx * w * dw
	;
	; get effective area
	earea = obsspec / rspho
	;
	; fit inverse sensitivity, effective area
	t=where(w ge wgoo0 and w le wgoo1, nt)
	if nt gt 0 then begin
		wf = w - min(w)
		sf = invsen
		wgt = (invsen - invsen) + 1.
		bad = where(w lt wgoo0 or w gt wgoo1, nbad)
		if nbad gt 0 then wgt[bad] = 0.
		pcwi_print_info,ppar,pre,'# pixels outside wl range',nbad,info=2
		;
		; iterative polynomial fit from 4-6 order
		for i=4,8 do begin
			res = polyfit(wf,sf,i,finvsen,weight=wgt)
			diff = sf - finvsen
			ims,diff,rmn,rsg,wgt
			if nbad gt 0 then wgt[bad] = 0.
			good = where(wgt gt 0., ngood)
			pcwi_print_info,ppar,pre,'Fit order, # good points',i,ngood, $
				info=2
		endfor
		fearea = (rsflx / finvsen ) / rspho
	endif else begin
		pcwi_print_info,ppar,pre,'no good wavelengths to fit',/error
	endelse
	;
	; plot effective inverse sensitivity
	if doplots then begin
		yrng = get_plotlims(invsen[gy])
		plot,w,invsen,title=sname+' Img #: '+strn(pcfg.imgnum), $
			xtitle='Wave (A)',xran=[wall0,wall1],/xs, $
			ytitle='Effective Inv. Sens. (erg/cm^2/A/e-)', $
			yran=yrng,/ys,xmargin=[11,3]
		oplot,w,finvsen,color=colordex('red')
		oplot,[wgoo0,wgoo0],!y.crange,color=colordex('green'),thick=3
		oplot,[wgoo1,wgoo1],!y.crange,color=colordex('green'),thick=3
		read,'Next: ',q
		;
		; plot effective area (cm^2)
		yrng = get_plotlims(earea)
		maxea = max(earea)
		mo = moment(earea)
		if area gt 0 then begin
			plot,w,earea, $
				title=sname+' Img #: '+strn(pcfg.imgnum)+tlab, $
				xtitle='Wave (A)',xran=[wall0,wall1],/xs, $
				ytitle='Effective Area (cm^2/A)',ys=9, $
				yran=yrng,xmargin=[11,8]
			oplot,w,fearea,color=colordex('red')
			oplot,[wgoo0,wgoo0],!y.crange,color=colordex('green'), $
				thick=3
			oplot,[wgoo1,wgoo1],!y.crange,color=colordex('green'), $
				thick=3
			oplot,!x.crange,[maxea,maxea],linesty=2
			oplot,!x.crange,[mo[0],mo[0]],linesty=3
			axis,yaxis=1,yrange=100.*(!y.crange/area),ys=1, $
				ytitle='Efficiency (%)'
		endif else begin
			plot,w,earea,title=sname+' Img #: '+strn(pcfg.imgnum), $
				xtitle='Wave (A)',xran=[wall0,wall1],/xs, $
				ytitle='Effective Area (cm^2/A)',yran=yrng,/ys
			oplot,w,fearea,color=colordex('red')
			oplot,[wgoo0,wgoo0],!y.crange,color=colordex('green'), $
				thick=3
			oplot,[wgoo1,wgoo1],!y.crange,color=colordex('green'), $
				thick=3
			oplot,!x.crange,[maxea,maxea],linesty=2
			oplot,!x.crange,[mo[0],mo[0]],linesty=3
		endelse
		read,'Next: ',q
	endif
	;
	; write out effective inverse sensitivity
	;
	; update invsens header
	sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
	sxaddpar,hdr,'INVSENS','T',' effective inv. sens. spectrum?'
	sxaddpar,hdr,'INVSY0',y0,' low wave pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSY1',y1,' high wave pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSX0',gx0,' low spatial pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSX1',gx1,' high spatial pixel for eff inv. sens.'
	sxaddpar,hdr,'INVSLMX',mxsl,' brightest std star slice'
	sxaddpar,hdr,'INVSL0',sl0,' lowest std star slice summed'
	sxaddpar,hdr,'INVSL1',sl1,' highest std star slice summed'
	sxaddpar,hdr,'INVSLX',cx,' spatial pixel position of std within slice'
	sxaddpar,hdr,'BUNIT','erg/cm^2/A/e-',' brightness units'
	sxaddpar,hdr,'EXPTIME',1.,' effective exposure time (seconds)'
	;
	; remove old WCS
	sxdelpar,hdr,'RADESYS'
	sxdelpar,hdr,'EQUINOX'
	sxdelpar,hdr,'LONPOLE'
	sxdelpar,hdr,'LATPOLE'
	sxdelpar,hdr,'NAXIS2'
	sxdelpar,hdr,'NAXIS3'
	sxdelpar,hdr,'CTYPE1'
	sxdelpar,hdr,'CTYPE2'
	sxdelpar,hdr,'CTYPE3'
	sxdelpar,hdr,'CUNIT1'
	sxdelpar,hdr,'CUNIT2'
	sxdelpar,hdr,'CUNIT3'
	sxdelpar,hdr,'CNAME1'
	sxdelpar,hdr,'CNAME2'
	sxdelpar,hdr,'CNAME3'
	sxdelpar,hdr,'CRVAL1'
	sxdelpar,hdr,'CRVAL2'
	sxdelpar,hdr,'CRVAL3'
	sxdelpar,hdr,'CRPIX1'
	sxdelpar,hdr,'CRPIX2'
	sxdelpar,hdr,'CRPIX3'
	sxdelpar,hdr,'CD1_1'
	sxdelpar,hdr,'CD1_2'
	sxdelpar,hdr,'CD2_1'
	sxdelpar,hdr,'CD2_2'
	sxdelpar,hdr,'CD3_3'
	;
	; set wavelength axis WCS values
	sxaddpar,hdr,'WCSDIM',1
	sxaddpar,hdr,'CTYPE1','AWAV',' Air Wavelengths'
	sxaddpar,hdr,'CUNIT1','Angstrom',' Wavelength units'
	sxaddpar,hdr,'CNAME1','PCWI INVSENS Wavelength',' Wavelength name'
	sxaddpar,hdr,'CRVAL1',w0,' Wavelength zeropoint'
	sxaddpar,hdr,'CRPIX1',crpixw,' Wavelength reference pixel'
	sxaddpar,hdr,'CDELT1',dw,' Wavelength Angstroms per pixel'
	;
	; write out inverse sensitivity file
	ofil = pcwi_get_imname(ppar,pcfg.imgnum,'_invsens',/nodir)
	pcwi_write_image,invsen,hdr,ofil,ppar
	;
	; update effective area header
	sxaddpar,hdr,'INVSENS','F',' effective inv. sens. spectrum?'
	sxaddpar,hdr,'EFFAREA','T',' effective area spectrum?'
	sxaddpar,hdr,'BUNIT','cm^2/A',' brightness units'
	sxaddpar,hdr,'CNAME1','PCWI EA Wavelength',' Wavelength name'
	;
	; write out effective area file
	ofil = pcwi_get_imname(ppar,pcfg.imgnum,'_ea',/nodir)
	pcwi_write_image,earea,hdr,ofil,ppar
	;
	return
end
