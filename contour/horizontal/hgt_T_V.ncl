;----------------------------------------------------------------------
; wrf_gsn_6.ncl
;----------------------------------------------------------------------
; Concepts illustrated:
;   - Using gsn_csm scripts to plot WRF-ARW data
;   - Overlaying filled contours, and vectors on a map
;   - Setting the correct WRF map projection using wrf_map_resources
;   - Setting lots of resources to customize a WRF plot
;----------------------------------------------------------------------
; This script is meant to show the difference between plotting WRF
; data using wrf_xxx scripts, and using gsn_csm_xxx scripts.
;----------------------------------------------------------------------
; In NCL Versions 6.3.1 and earlier, you will get these warnings which
; you can safely ignore:
;
; warning:start_lat is not a valid resource in wrf_gsn_contour at this time
; warning:start_lon is not a valid resource in wrf_gsn_contour at this time
; warning:end_lat is not a valid resource in wrf_gsn_contour at this time
; warning:end_lon is not a valid resource in wrf_gsn_contour at this time
; warning:mpNestTime is not a valid resource in map at this time
;----------------------------------------------------------------------
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

begin
  hour = (/"00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"/)
  day  = (/"09","10","11"/)
  start_hour = 18
  run_hour   = 42

  domain=1
  hr = start_hour
  hr_BJ = hr+8
  d = 0
  d_BJ = 0
  do h=1,run_hour
  
  if hr_BJ.ge.24 then 
  hr_BJ = 0
  d_BJ =d_BJ+1
  end if

  if hr.ge.24 then
  hr=0
  d=d+1
  end if
  
;---Open WRF output file
  filename = "wrfout_d0"+domain+"_2019-12-"+day(d)+"_"+hour(hr)+":00:00"
  a        = addfile("../"+filename,"r")

;---Read several WRF variables at first time step
  it  = 0
  ;tc  = wrf_user_getvar(a,"tc",it)    ; 3D temperature
  t = wrf_user_getvar(a,"T2",it)
  u   = wrf_user_getvar(a,"ua",it)    ; 3D U at mass points
  v   = wrf_user_getvar(a,"va",it)    ; 3D V at mass points
  ter = wrf_user_getvar(a,"HGT",it)   ; terria height

;---Now get the lowest (bottommost) level
  nl  = 0
  u10 = u(nl,:,:)
  v10 = v(nl,:,:)
  t2 = t
  t2(:,:) = t(:,:)-272.15
;---Change the metadata
  t2@description = "2m Temperature"
  t2@units       = "deg C"
  u10@units       = "m s-1"
  v10@units       = "m s-1"
  filename_output = "d0"+domain+"_2019-12-"+day(d_BJ)+"_"+hour(hr_BJ)+":00:00"
  wks = gsn_open_wks("png","hgt_T/hgt_T_"+filename_output)
  ;wks = gsn_open_wks("png","hgt_V/hgt_V_"+filename_output)

;---Set common resources for all plots
  res                = True
  res@gsnFrame       = False
  res@gsnDraw        = False
  res@gsnLeftString  = ""
  res@gsnRightString = ""

;---Necessary for contours to be overlaid correctly on WRF projection
;  res@tfDoNDCOverlay        = True
  res@tfDoNDCOverlay        = "NDCViewport" ; can use this in NCL V6.5.0 or later
   
;---Temperature filled contour plot
  tc_res                      = res
  tc_res@cnFillOn             = True
  ;tc_res@cnFillPalette        = "cmp_b2r"
  tc_res@cnFillPalette = "MPL_coolwarm"
  tc_res@cnLevelSelectionMode = "ExplicitLevels"
  tc_res@cnLevels             = ispan(-10,10,1)
  tc_res@cnInfoLabelOn        = False
  tc_res@cnLineLabelsOn       = False
  tc_res@cnLinesOn            = False
  tc_res@cnFillDrawOrder      = "PreDraw"
  tc_res@lbOrientation        = "Vertical"
  tc_res@lbLabelFontHeightF   = 0.01

  contour_tc = gsn_csm_contour(wks,t2,tc_res)

;---Wind vector plot
  vec_res                       = res
  vec_res@vcMinDistanceF        = 0.025
  vec_res@vcRefMagnitudeF       = 5
  vec_res@vcRefLengthF          = 0.038
  vec_res@vcMinFracLengthF      = 0.0
  vec_res@vcLineArrowThicknessF = 3.0
  vec_res@vcGlyphStyle          = "LineArrow"
  vec_res@vcRefAnnoOn           = True
  vec_res@vcRefAnnoSide         = "Top"
  vec_res@vcRefAnnoString1On    = "True"
  vec_res@vcRefAnnoString1      = "5 m s-1"
  vec_res@vcRefAnnoPerimOn      = False
  ;vec_res@vcRefAnnoZone         = 3
  vec_res@vcRefAnnoOrthogonalPosF       = -0.04
  if (domain.eq.5).or.(domain.eq.8) then
    vec_res@vcRefMagnitudeF       = 3
    vec_res@vcRefAnnoString1      = "3 m s-1"
  end if
  vector = gsn_csm_vector(wks,u10,v10,vec_res)

;---Terria plot
  ter_res                      = res
  ter_res@cnFillOn             = True
  ter_res@cnFillPalette        = "cmocean_turbid"
  ter_res@lbOrientation        = "Vertical"
  ter_res@lbLabelFontHeightF   = 0.01
 
  contour_ter = gsn_csm_contour(wks,ter,ter_res)

;---Map plot
  map_res                      = True
  map_res@gsnFrame             = False
  map_res@gsnDraw              = False
  map_res@tiMainString         = filename_output
  ;map_res@gsnLeftString        = tc2@description + " (" + tc2@units + ")"
  map_res@gsnLeftString        = t2@description + " (" + t2@units + ")"
  ;map_res@gsnRightString       = "Wind (" + u10@units + ")"
  map_res@gsnStringFontHeightF = 0.015

;---Set map resources based on projection on WRF output file
  map_res = wrf_map_resources(a,map_res)
  map_res@mpFillOn                    = False
  map_res@mpOutlineOn                 = True
  map_res@mpDataBaseVersion           = "HighRes"
  map_res@mpOutlineDrawOrder          = "PostDraw"
  map_res@mpUSStateLineColor          = "Black"
  map_res@mpPerimLineColor            = "Black"
  map_res@mpNationalLineColor         = "Black"
  map_res@mpLimbLineColor             = "Black"
  map_res@mpGridLineColor             = "Black"
  map_res@mpGeophysicalLineColor      = "Black"
  map_res@mpUSStateLineThicknessF     = 3.0
  map_res@mpNationalLineThicknessF    = 3.0
  map_res@mpGeophysicalLineThicknessF = 3.0

  map = gsn_csm_map(wks,map_res)
 ; map= wrf_map(wks,a,map_res)
      mkres  = True
      mkres@gsMarkerIndex = 16
      mkres@gsMarkerColor = "Black"
      mkres@gsMarkerSizeF = 8
      txres  = True
      txres@txFontHeightF = 0.020
      txres@txFont        = "helvetica" ;"times-roman"
      txres@txFontColor = "Black"

     dum_cities8  = gsn_add_polymarker(wks,map,115.50,38.80,mkres)     ;baoding
     dum_cities21 = gsn_add_polymarker(wks,map,115.12,39.03,mkres)    ;tuonan
     dum_cities26 = gsn_add_polymarker(wks,map,116.10,38.74,mkres)    ;renqiu
  if (domain.eq.1) then
     dum8  = gsn_add_text(wks,map,"Baoding",115.50,38.70,txres)
     dum21 = gsn_add_text(wks,map,"Tuonan",115.12,38.93,txres)
     dum26 = gsn_add_text(wks,map,"Renqiu",116.10,38.50,txres)
  end if

;---Overlay plots on map and draw.
  ;overlay(map,contour_ter)
  overlay(map,contour_tc)
  overlay(map,vector)

  draw(map)   ; This will draw all overlaid plots and the map
  frame(wks)

hr=hr+1
hr_BJ=hr_BJ+1
end do 
end

