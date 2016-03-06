; $Id: startup.pro | Mon Apr 20 09:02:02 2015 -0700 | Don Neill  $
;
; PCWI DRP IDL startup file
;
; Astrolib setup
astrolib
;
; plotting system variables
defsysv,'!top_color',0
defsysv,'!top_colorx',0
defsysv,'!top_colorps',255
defsysv,'!colorstr',''
defsysv,'!colorstr_ps',''
defsysv,'!colorstr_x',''
;
; PCWI system variables:
; EDIT THIS to point to data dir in current release!!!
defsysv,'!PCWI_DATA','/Users/neill/pcwi/pderp/data/'
;
