load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
begin
  hour = (/"00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"/)
  day  = (/"09","10","11"/)
  start_hour = 18
  run_hour   = 42

  domain=8
  hr = start_hour
  hr_BJ = hr+8
  d = 0
  d_BJ = 0
  t = new((/60,56,run_hour/),float)
  t_point = new((/60*run_hour,56/),float)
  ;t2 = new((/60,45/),float)
  ;t2_point = new((/60/),float)
;---one by one file--------------------------------------------------
  do h=1,run_hour

  if hr_BJ.ge.24 then
  hr_BJ = 0
  d_BJ=d_BJ+1
  end if

  if hr.ge.24 then
  hr=0
  d=d+1
  end if

;---Open WRF output file
  filename_a = "../wrfout_d0"+domain+"_2019-12-"+day(d)+"_"+hour(hr)+":00:00"
  a        = addfile(filename_a,"r")

;---Station information
  ;stnm = "baoding"
  ;lat_o = 38.80
  ;lon_o = 115.50
  stnm = "renqiu"
  lat_o=38.74
  lon_o=116.10
  loc = wrf_user_ll_to_xy(a,lon_o,lat_o,True)

;---Read several WRF variables
  tc = wrf_user_getvar(a,"tc",-1)
  ;t2_0 = wrf_user_getvar(a,"T2",-1)
  t(:,:,h-1) = tc(:,:,loc(1),loc(0))  ;---h=1,*,file number=0,h-1
  ;t2(:,h-1) = t2_0(:,loc(1),loc(0))
;---end do one by one file
  delete(tc)
  hr=hr+1
  hr_BJ=hr_BJ+1
  end do
;----------------------------------------------------------------------
;---Read Times for all files
  filename_b = systemfunc ("ls "+"../"+"wrfout_d0"+domain+"_2019-12-"+"*")
  b        = addfiles(filename_b,"r")
  times_in_file = wrf_user_getvar(b,"Times",-1)
  dims = dimsizes(times_in_file)
  times = ispan(0,dims(0)-1,1)
printVarSummary(times)
;---Read height only once
  filename_c = "../wrfout_d0"+domain+"_2019-12-"+day(1)+"_"+hour(start_hour)+":00:00"
  c        = addfile(filename_c,"r")
  height0 = wrf_user_getvar(c,"height",-1)
  height = height0(0,:,loc(1),loc(0))
  height_label = ispan(0,2000,10)

;---Reshape 3D (times,bottom_top,file) to 2D (times,bottom_top)
  do i = 1,run_hour
    do j = 1,60
    t_point((i-1)*60+j-1,:) = t(j-1,:,i-1)
    ;t2_point((i-1)*60+j-1) = t2(j-1,i-1)
    end do
  end do
  ;do i = 0,2699
  ;opt = True
  ;opt@fout = "tc"
  ;write_matrix (tt(:,:), "56f10.2", opt)
  ;end do
  ;printVarSummary(tt)
  ;t_point_m = t_point(Time|:,bottom_top|0:40)
  t_point_m = new((/60*run_hour,dimsizes(height_label)/),float)

  do i = 0,60*run_hour-1
    t_point_m(i,:) = wrf_interp_1d(t_point(i,:),height,height_label)
  end do
  ;t_point_m(:,0) = t2_point(:)
  ;print(t_point_m)
  
;---Reshape (times,bottom_top) to (times,height in m)
  t_point_m!0 = "times"
  ;t_point_m&times = times
  ;t_point_m!1 = "height"
  ;t_point_m&height = height_label

;---Open a workstation
  wks = gsn_open_wks("png","tc_times_height_d0"+domain+"-"+stnm)
  taus = (/ 0, 360, 720, 1080, 1440, 1800, 2160, 2520 /)       ; create a time reference
  labels = (/"10_02","10_08","10_14","10_20","11_02","11_08","11_14","11_20"/)  ;in BJ time
  res = True                            ; Set basic resources
  res@gsnDraw              = False      ; Don't draw individual plot.
  res@gsnFrame             = False      ; Don't advance frame.
  res@vpXF                 = 0.15       ; x location
  res@vpYF                 = 0.90       ; y location
  res@vpWidthF             = 0.70       ; width
  res@vpHeightF            = 0.40       ; height
  ;res@tiXAxisString        = "Day_Time"
  ;res@tiXAxisFontHeightF   = 0.016
  res@tmXBLabelJust        = "CenterCenter"
  res@tmXBLabelFontHeightF = .016
  res@tmXBMode         = "Explicit"
  res@tmXBValues       = taus
  res@tmXBLabels       = labels
  res@tmXTOn           = False
  res@tmYLMode         = "Explicit"
  ;res@tmYLValues       = (/4,8,12,16,20,24,28,32,36,40/)
  res@tmYLValues       =(/20,40,60,80,100,120,140,160,180,200/)
  res@tmYLLabels       = (/"200","400","600","800","1000","1200","1400","1600","1800","2000"/)
  res@tmYLLabelFontHeightF = .016
  res@tmYROn           = False
  res@gsnSpreadColors  = True          ; use full range of colors
  res@gsnLeftString    = ""
  res@gsnRightString   = "Temperature: deg C"
  res@cnFillOn         = True          ; turns on color fill
  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels             = ispan(-10,10,1)
  res@lbOrientation    = "vertical"      ; vertical label bar
  plot                 = gsn_csm_contour(wks,t_point_m(bottom_top|0:200,times|:),res)

  draw(plot)
  frame(wks)
end
