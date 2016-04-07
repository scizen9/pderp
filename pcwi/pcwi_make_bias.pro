; $Id: pcwi_make_bias.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
; Copyright (c) 2016, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	PCWI_MAKE_BIAS
;
; PURPOSE:
;	This procedure creates a median-stacked bias from a list of bias
;	image numbers given in PCWI_PPAR struct PPAR.BIASES.
;
; CATEGORY:
;	Data reduction for the Palomar Cosmic Web Imager (PCWI).
;
; CALLING SEQUENCE:
;	PCWI_MAKE_BIAS, ppar
;
; INPUTS:
;	ppar	- PCWI_PPAR struct for bias group to combine
;
; KEYWORDS:
;	None
;
; PROCEDURE:
;	Uses list of bias frames to construct a master bias frame that
;	is median stacked.
;
; EXAMPLE:
;
;	This will create a master bias frame.
;
;	Ppar = PCWI_READ_PPAR('redux/mbias_0.ppar')
;	PCWI_MAKE_BIAS,Ppar
;
; MODIFICATION HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2013-APR-22	Initial version
;	2013-MAY-08	remove SKIP1 keyword and now use PCWI_PPAR as input
;	2013-JUL-03	added logging
;	2013-SEP-14	use ppar to pass loglun
;	2016-MAR-03	Split from kderp to pderp
;-
pro pcwi_make_bias,ppar
	;
	; initialize
	pre = 'PCWI_MAKE_BIAS'
	;
	; check input ppar
	if pcwi_verify_ppar(ppar,/init) ne 0 then return
	;
	; log
	pcwi_print_info,ppar,pre,systime(0)
	;
	; are there biases listed?
	if strlen(ppar.biases) le 0 then begin
		pcwi_print_info,ppar,pre,'no biases entered',/error
		return
	endif
	;
	; get image list
	rangepar,ppar.biases,bnums
	nb = n_elements(bnums)
	;
	; are there enough?
	if nb ge ppar.mingroupbias then begin
		;
		; log number of images
		pcwi_print_info,ppar,pre,'combining this many images',nb
		;
		; set up image stack
		im = pcwi_read_raw(ppar,bnums[0],header=hdr,/silent)
		nx = sxpar(hdr,'NAXIS1')
		ny = sxpar(hdr,'NAXIS2')
		stack = intarr(nb,nx,ny)
		stack[0,*,*] = im
		nstack = 1
		for i=1,nb-1 do begin
			im = pcwi_read_raw(ppar,bnums[i],header=bhdr, $
					/silent)
			if strtrim(sxpar(bhdr,'IMGTYPE'),2) eq 'bias' then begin
				stack[nstack,*,*] = im
				nstack = nstack + 1
			endif else begin
				pcwi_print_info,ppar,pre,'not a bias image', $
					bnums[i],/warning
				bnums[i] = -1
			endelse
		endfor
		;
		; adjust stack if needed
		if nstack lt nb then begin
			stack = stack[0:(nstack-1),*,*]
			pcwi_print_info,ppar,pre, $
				'stack adjusted for missing bias'
			rangepar,bnums[where(bnums ge 0)],rl
		endif
		;
		; print report if requested
		pcwi_print_info,ppar,pre,'Master Bias has '+strn(nstack)+ $
			' images'
		;
		; create master bias from median stack
		mbias = fltarr(nx,ny)
		for yi=0,ny-1 do for xi=0,nx-1 do $
			mbias[xi,yi] = median(stack[*,xi,yi])
		;
		; get read noise for each amplifier
		;
		; first, get ccd geometry
		pcwi_map_ccd,hdr,asec,bsec,csec,tsec,namps=namps,verbose=ppar.verbose
		mbias_rn = fltarr(namps)
		for ia = 0, namps - 1 do begin
			x0 = csec[ia,0,0]
			x1 = csec[ia,0,1]
			y0 = csec[ia,1,0]
			y1 = csec[ia,1,1]
			noise = reform(stack[1,x0:x1,y0:y1] - $
				       stack[2,x0:x1,y0:y1])
			ims,noise,mn,sg
			;
			; get gain
			gain = sxpar(hdr,'GAIN'+strn(ia+1))
			mbias_rn[ia] = gain * sg/sqrt(2)
			;
			; log
			pcwi_print_info,ppar,pre,'Amp'+strn(ia+1)+ $
				' Read noise in e-',mbias_rn[ia]
			;
			; update header
			sxaddpar,hdr,'BIASRN'+strn(ia+1),mbias_rn[ia], $
				' amp'+strn(ia+1)+' RN in e- from bias'
		endfor
		;
		; update master bias header
		sxaddpar,hdr,'HISTORY','  '+pre+' '+systime(0)
		sxaddpar,hdr,'NMEDIAN',nstack, $
			' number of images used for median stack'
		sxaddpar,hdr,'MASTBIAS','T', ' master bias image?'
		sxaddpar,hdr,'BIASLST',ppar.biases, $
			' range list of image numbers for stack'
		if ppar.biasskip1 then $
			sxaddpar,hdr,'BIASKP1','T', ' skip first bias image?' $
		else	sxaddpar,hdr,'BIASKP1','F', ' skip first bias image?'
		;
		; write out image file
		ofile = ppar.masterbias
		pcwi_write_image,mbias,hdr,ofile,ppar
	endif else $
		pcwi_print_info,ppar,pre,'too few bias frames',nb,/error
	;
	return
end
