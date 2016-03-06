;$Id: pcwi_read_raw.pro | Wed Mar 4 12:02:01 2015 -0800 | Don Neill  $
;
;+
;
;-
function pcwi_read_raw,ppar,imgno,header=header,silent=silent
	;
	; initialize
	pre = 'PCWI_READ_RAW'
	;
	; check input
	if pcwi_verify_ppar(ppar,/init) ne 0 then return,-1
	;
	; create file name
	fspec = ppar.rawdir + ppar.froot + $
		string(imgno,form='(i0'+strn(ppar.fdigits)+')')+'.fit*'
	flist = file_search(fspec,count=nf)
	;
	; check results
	if nf ne 1 then begin
		print,pre+': Error - file not found or ambiguous: ',imgno
		header=''
		return,-1
	endif
	im = mrdfits(flist[0],0,header,silent=silent,/unsigned)
	;
	return,im
end
