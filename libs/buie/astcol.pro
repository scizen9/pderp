;+
; NAME:
;  astcol
; PURPOSE:
;  Collect astrometry observations for multiple objects
; DESCRIPTION:
;  This program takes care of assigning a unique code to moving target
;    astrometric observations.  But, there may already be a cross-reference
;    file for some observations.  These are applied at the same time.
;
;  The current directory is scanned for all files ending in .ast.  These
;    are the raw astrometry files.  Second, a file name lplast.xrft is
;    sought.  This file will contain cross-references between the raw
;    ids implied by the raw astrometry files and some other unique
;    designation.  Anything that does not have an entry in the
;    cross-reference file needs a final code which is optionally
;    generated by this program.  If you request file codes, another
;    file is consulted, newobj.dat in the SAVEDIR directory, and a unique
;    id is generated and added to the lplast.xrft file.
;
;  The newobj.dat file is searched for the first properly formatted line.
;    This object code of this is used as a template for further codes.
;    An object code consists of one or more alphabetic characters followed
;    by up to five numeric digits.  Usually, there are two leading
;    alphabetic characters but this program doesn't mind finding more.
;    If there is one only alphabetic character then the limit is 99999
;    object codes in the file.  If you have two or more alphabetic
;    characters, the last digit of these will be used as an additional
;    sequence character starting at 'A' (regardless of what may already
;    be in the file).  This increases the limit to 2,600,000 (less 26,
;    0 is not used).
;
;  Once all the cross-referencing and code assignment is complete, a series
;    of files is written to SAVEDIR.  YYMMDD is the date which is generated
;    from the name of the current directory, usually meant to be the UT date
;    of observation (but is not required, any name will do)
;      SAVEDIR/YYMMDD.ast  - astrometry file (my format)
;      SAVEDIR/YYMMDD.ted  - astrometry file (Ted Bowell format)
;      SAVEDIR/YYMMDD.kted - astrometry file of slow moving objects (Bowell)
;      SAVEDIR/YYMMDD.info - summary listing of all objects in ast/ted file
;      SAVEDIR/YYMMDD.kinfo - summary list of all objects in kted file.
;
;  Note that the objects are placed into the ast and ted files in order of
;    increasing rate of motion.  Also note that generating final codes can
;    be hard to undo so only do it if you are absolutely sure you are ready.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astcol
;
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DES     - Flag, if set will alter the list of objects sent to the kinfo,
;              and kted files.  Without the flag, the files contain slow
;              moving objects.  With the flag, a query is done against the
;              des.vclass table to find a list of interesting objects.  To
;              this list is also added any slow moving object that is at a
;              solar elongation of 150 degrees or greater.  These slow
;              moving objects are NOT incorporated into the .ted file.
;  NONEWCODE - Flag, if set suppresses the generation of new object id codes.
;  NOXREF  - Flag, if set will not use available cross-reference information.
;  OBSCODE - Observatory code for observations, default='688' (Lowell Obs.)
;              THIS IS IMPORTANT!  So much so that it probably should not
;              be a keyword input parameter.
;  SAVEDIR - Directory where final astrometry is to be placed.  The default
;              value is /net/frakir/raid/buie/astrometry
;  NODB    - Flag, if set will suppress saving new codes to des.newobj database.
;              This flag has no effect if not setting final codes.
;  REPOST  - Flag, if set will check to make sure the localid code is present
;              in the des.newobj database.  If it is not found, it will be
;              (re)posted to the database.  NODB will override this keyword.
;              You must also enable final codes to be able to repost the data.
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
;  DO NOT SET SAVEDIR TO BE THE SAME AS THE CURRENT DIRECTORY!!!
;
;  This program reads all the .ast files in the current directory and then
;    writes files to SAVEDIR among which is one file with a .ast suffix.  This
;    output file is NOT the same format as the input .ast files.  If you
;    accidentally make this mistake, then the SECOND time you run astcol,
;    it will pick up the output file and become very unhappy.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    97/03/18 - Written by Marc W. Buie, Lowell Observatory
;    97/07/09 - Added the savedir keyword
;    97/07/25, MWB, slight change to the Bowell file format output.
;    98/06/22, MWB, added call to ASTLIST at end, also newcodes are taken from
;                 first available number, not last code in file.
;    98/11/04, MWB, now filters out magnitudes fainter than 80.0
;    2000/03/09, MWB, moved creation of .ted file to ast2ted.pro
;    2000/09/20, MWB, changed to support vectors with repwrite.
;    2002/09/09, MWB, added support for string obscode values
;    2003/05/02, MWB, added NOXREF keyword
;    2003/08/15, MWB, added NODB keyword and database storage to des.newobj
;    2004/02/09, MWB, changed SAVEDIR default path
;    2004/02/27, MWB, changed the codetag algorithm to extend the sequence
;    2004/07/24, MWB, added the DES flag
;  2004/9/21, MWB, removed obsolete call to Findfile
;  2005/03/31, MWB, added REPOST keyword
;  2005/10/12, MWB, fixed bug that prevented saving slow moving objects to a
;                     separate file if the mysql-based list was empty
;  2005/11/28, MWB, added code to make SAVEDIR if it doesn't exist but only if the
;                     user requests that the program try to make it.
;-
pro astcol,OBSCODE=obscode,SAVEDIR=savedir,NONEWCODE=nonewcode,NOXREF=noxref, $
   NODB=nodb,DES=des,REPOST=repost

   self='ASTCOL: '

   if badpar(obscode,[0,1,2,3,7],0,CALLER=self+'(OBSCODE) ', $
                default='688',type=codetype) then return

   if badpar(savedir,[0,7],0,CALLER=self+'(SAVEDIR) ', $
                default='/net/frakir/raid/buie/astrometry/') then return
   savedir=addslash(savedir)

   ; Check for output directory, if not present, ask if caller wants to create it.
   if not exists(savedir) then begin
      ans=''
      print,savedir+' does not exist.'
      read,prompt='Do you wish to create the directory? (default=no) ',ans
      if strlowcase(strmid(ans,0,1)) eq 'y' then begin
         file_mkdir,savedir
      endif
      if not exists(savedir) then begin
         print,self,'Aborting, no output directory to write to.'
         return
      endif
   endif

   if badpar(nonewcode,[0,1,2,3],0,CALLER=self+'(NONEWCODE) ', $
             default=0) then return
   if badpar(noxref,[0,1,2,3],0,CALLER=self+'(NOXREF) ',default=0) then return
   if badpar(nodb,[0,1,2,3],0,CALLER=self+'(NODB) ',default=0) then return
   if badpar(des,[0,1,2,3],0,CALLER=self+'(DES) ',default=0) then return
   if badpar(repost,[0,1,2,3],0,CALLER=self+'(REPOST) ',default=0) then return

   ; deal with the observatory code, it can be either a string or a number.
   ;   the number input is a legacy form and can be only a three digit number.
   ;   if it is a number, then convert to a 3-digit string
   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   ; Get current directory, this will identify the date of observation.
   cd,current=cdir
   pos=strpos(cdir,'/',/reverse_search)
   date = strmid(cdir,pos+1,99)

   ; useful for the mysql command construction
   c=','

   ; open the database connection if needed
   if not nodb or des then openmysql,dblun,'des'

   ; if DES flag set, we need to get the "interesting" list from the vclass
   ;   database.
   if des then begin
      yr = fix(strmid(date,0,2))
      if yr gt 90 then yr += 1900 else yr += 2000
      rundate=strn(yr)+'-'+strmid(date,2,2)+'-'+strmid(date,4,2)
      cmd = ['select lookerid from vclass where vclass.des=1 and', $
             'rundate='+quote(rundate)+';']
      mysqlquery,dblun,cmd,desobj,format='a'
   endif

   ; Look for a cross-reference file.
   final = exists('lplast.xrft') and not noxref

   ; in this case, we need to assign final local identification codes
   ;   for each object that doesn't already have a cross-reference.
   if final then begin

      ; Read in the cross-reference file.  By default, this routine will
      ;   read lplast.xrft from the current directory.  This is the file
      ;   that will ultimately be modified with the new assignments.
      rdlplast,tmpid,realid,nxrf,/trim

      ; if the keyword was not set, ask if now is the time to setup 
      ;    final id codes.
      if not nonewcode then begin
         ans=''
         read,'Do you want to setup final ID codes? (def=no) ',ans
         if strmid(ans,0,1) ne 'y' then nonewcode = 1
      endif

      ; all of the inputs and options indicate that it's time to do the
      ;   new object codes now.
      if not nonewcode then begin

         ; Locate the id code file and load it
         ncodes = 0L
         codetag = '?'
         if exists(savedir+'newobj.dat') then begin

            openr,lun,savedir+'newobj.dat',/get_lun

            ; count the number of lines in the file
            line=''
            while not eof(lun) do begin
               readf,lun,line,format='(a1)'
               ncodes = ncodes+1L
            endwhile

            ; rewind file
            point_lun,lun,0L

            ; setup arrays for the information from the id code file
            tagid=strarr(ncodes)
            lines=strarr(ncodes)

            ; Read the id code file.  Lines contains the entire line from
            ;   the file and tagid is the first word of that line.
            for i=0L,ncodes-1 do begin
               readf,lun,line,format='(a)'
               lines[i]=line
               words=strsplit(line,/extract)
               tagid[i]=words[0]
            endfor
            free_lun,lun

            ; scan through the information read from the file.  A valid
            ;  code tag looks like XXXxxxx where XXX is 1 or more alphabetic
            ;  characters and xxxx is 1 or more numeric digits.  We're looking
            ;  for the first valid codetag, save it and move on.
            i=0
            repeat begin
               if tagid[i] ne '' then begin
                  j=0
                  repeat begin
                     ok=valid_num(strmid(tagid[i],j,99),code,/integer)
                     if ok and j ne 0 then $
                        codetag = strmid(tagid[i],0,j)
                     j=j+1
                  endrep UNTIL j eq strlen(tagid[i]) or ok
               endif
               i=i+1
            endrep UNTIL codetag ne '?' or i eq ncodes

            print,'initial codetag=',codetag

            ; error trap, must have seen a valid codetag
            if codetag eq '?' then begin
               print,'Sorry, this file does not have a clearly defined tag code.'
               print,'Unable to continue.'
               if not nodb and not nonewcode then free_lun,dblun
               return
            endif

            ; separate out the alphabetic part of the code
            prefix=strarr(ncodes)
            for i=0L,ncodes-1 do begin
               prefix[i]=strmid(tagid[i],0,strlen(codetag))
            endfor

            ; strip off the numeric part of the codetag.  Note that it is
            ;   assumed that the alphabetic part of the codetag is the same
            ;   length for all entries.
            tagnum=lonarr(ncodes)
            for i=0L,ncodes-1 do begin
               tagnum[i]=long(strmid(tagid[i],strlen(codetag),99))
            endfor

            ; further split up the codetag into two parts, the last character
            ;   is used as an additional sequence id and is incremented from
            ;   'A' through the alphabet.  The rest of the codetag is used
            ;   as is.  This means codetag must have at least two characters
            ;   to use the extra sequence.
            if strlen(codetag) gt 1 then begin
               codetagbase = strmid(codetag,0,strlen(codetag)-1)
               codetagseq  = 'A'
            endif else begin
               codetagbase = codetag
               codetagseq  = ''
            endelse

            print,'codetag found is ',codetag,', codetagbase=',codetagbase

            ; take the list and resort by the tagnumber.
            idx=sort(tagnum)
            tagid  = tagid[idx]
            prefix = prefix[idx]
            lines  = lines[idx]
            tagnum = tagnum[idx]

         endif

      endif ; end of assigning new codes

   ; no new codes, automatically suppress saving to database
   endif else begin
      nodb=1
   endelse

   ; Get a list of all astrometry files to process
   filelist = file_search('*.ast',count=nfiles)

   if nfiles eq 0 then begin
      print,'No astrometry files to collect.  Aborting.'
      if not nodb and not nonewcode then free_lun,dblun
      return
   endif

   ; setup some useful variables.
   line=''
   blanks='                   '
   tab = string( byte(9) )

   ; build all of the output file names
   outast  = savedir+date+'.ast'
   outted  = savedir+date+'.ted'
   outkted  = savedir+date+'.kted'
   outinfo = savedir+date+'.info'
   outkinfo = savedir+date+'.kinfo'

   ; open the combined astrometry file for output
   openw,lunast,outast,/get_lun,width=200

   ; When new codes are being defined, the new stuff is accumulated to
   ;  a set of string arrays and then written out at the end.
   nxrft = 0   ; number of lines to add to lplast.xrft
   nncod = 0   ; number of lines to add to newobj.dat

   nobs=0
   istr=''
   ; loop over all of the astrometry files
   for i=0L,nfiles-1 do begin
   
      ; figure out the object name from the astrometry file.  If it starts
      ;   with 'a', then it's not a local reference code and the 'a' is
      ;   then stripped.
      obj=strsplit(filelist[i],'.',/extract)
      obj=obj[0]
      newobj=0
      if strmid(obj,0,1) eq 'a' then obj=strmid(obj,1,99)
      obj=strupcase(obj)

      ; Process the data for this object
      openr,lun,filelist[i],/get_lun

      ; if determining final codes, check out the name to see if it needs
      ;   a code generated and if so, find the first available code and
      ;   assign it to this object.
      if final then begin

         ; save original object naem
         origobj = obj

         ; is the object name already in the cross reference file?
         z=where(origobj eq tmpid,count)

         ; object is already in cross reference file, don't really do anything
         if count ne 0 then begin
            obj = realid[z[0]]
            istr= ''

         ; ok, this object needs a new code.
         endif else if not nonewcode then begin

            ; Get the next valid code number
            if codetagseq eq '' then begin
               z=lindgen(n_elements(tagnum))
               npcodes = ncodes
            endif else begin
               repeat begin
                  z=where(prefix eq codetagbase+codetagseq,count)
                  if count eq 99999L then codetagseq=string(byte(codetagseq)+1B)
               endrep until count lt 99999L
               npcodes = count
            endelse

            if z[0] eq -1 then begin
               newcode = 1
               obj = codetagbase+codetagseq+strn(newcode)
               istr = ' new starting code '+obj
            endif else if npcodes eq 1 then begin
               newcode = tagnum[z[0]]+1
               obj = codetagbase+codetagseq+strn(newcode)
               istr = ' new next code '+obj
            endif else begin
               diff = tagnum[z[1:npcodes-1]]-tagnum[z[0:npcodes-2]]
               zdiff = where(diff ne 1,count)
               if count eq 0 then begin
                  newcode = tagnum[z[npcodes-1]]+1
                  obj = codetagbase+codetagseq+strn(newcode)
                  istr = ' new end code '+obj
               endif else begin
                  newcode = tagnum[z[zdiff[0]]]+1
                  obj = codetagbase+codetagseq+strn(newcode)
                  istr = ' new code '+obj+' between ' + $
                         strn(tagnum[z[zdiff[0]]])+' and ' + $
                         strn(tagnum[z[zdiff[0]+1]])
               endelse
            endelse

            tag = strmid(origobj+blanks,0,8)
            line = '*         '+obj
            if nxrft eq 0 then begin
               xtag  = tag
               xinfo = tag+line
            endif else begin
               xtag  = [xtag,tag]
               xinfo = [xinfo,tag+line]
            endelse
            nxrft = nxrft + 1

            tagnum = [tagnum,newcode]
            prefix = [prefix,codetagbase+codetagseq]
            idx    = sort(tagnum)
            tagnum = tagnum[idx]
            prefix = prefix[idx]
            ncodes = ncodes+1

            newobj=1

         endif
         if des then begin
            zd = where(origobj eq desobj,count)
            if count eq 1 then desobj[zd[0]]=obj
         endif
         print,origobj,obj,istr,format='(a,1x,a,1x,a)'
      endif ; end of final block

      ; Read through file and add to the ast output file
      j=0
      while not eof(lun) do begin
         readf,lun,line,format='(a)'
         words=strsplit(line,/extract)
         if j eq 0 then firstfile = words[0]
         printf,lunast,line,' ',obscode,' ',obj
         nobs=nobs+1
         j=j+1
      endwhile
      free_lun,lun

      if final and newobj then begin
         if nncod eq 0 then begin
            tobj  = obj
            tinfo = obj+tab+firstfile
         endif else begin
            tobj  = [tobj,obj]
            tinfo = [tinfo,obj+tab+firstfile]
         endelse
         cmd='insert into newobj values ('+quote(obj)+c+quote(firstfile)+ $
             ',NULL,NULL,NULL,NULL,'+quote('Primary')+');'
         if not nodb then mysqlcmd,dblun,cmd,answer,nlines
         nncod = nncod + 1
      endif

      ; repost data, check if present, if not, add to newobj database
      if repost and not nodb then begin
         if strmid(obj,0,1) eq codetagbase then begin
            cmd='select localid from newobj where localid='+quote(obj)+';'
            mysqlcmd,dblun,cmd,answer,nlines
            if nlines eq 1 then begin
               cmd='insert into newobj values ('+quote(obj)+c+ $
                   quote(firstfile)+ $
                   ',NULL,NULL,NULL,NULL,'+quote('Primary')+');'
               mysqlcmd,dblun,cmd,answer,nlines
               print,'repost ',obj
            endif
         endif
      endif

   endfor

   free_lun,lunast
   if not nodb or des then free_lun,dblun

   if nxrft ne 0 then begin
      print,'Update lplast.xrft file with ',strn(nxrft),' new entries.'
      repwrite,'lplast.xrft',xtag,xinfo
   endif

   if nncod ne 0 then begin
      print,'Update newobj.dat file with ',strn(nncod),' new entries.'
      repwrite,savedir+'newobj.dat',tobj,tinfo,/nosort
   endif

   print,'Astrometry saved: ',date,', ',strtrim(string(nfiles),2), $
         ' objects and ',strtrim(string(nobs),2),' measurements.'

   print,'Creating Ted format output file.'
   ast2ted,outast,outted,outkted,DESOBJ=desobj
   astlist,outast,outinfo,outkinfo,DESOBJ=desobj

   if des then begin
      outobj  = savedir+date+'.kobj'
      idx=sort(desobj)
      openw,lun,outobj
      for i=0,n_elements(desobj)-1 do begin
         if desobj[idx[i]] ne '' then printf,lun,desobj[idx[i]]
      endfor
      free_lun,lun
   endif

end
