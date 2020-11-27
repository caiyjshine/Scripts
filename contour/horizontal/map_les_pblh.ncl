begin
  hour = (/"00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"/)
  day  = (/"09","10","11"/)
  start_hour = 18
  run_hour   = 43

  domain=8
  hr = start_hour
  hr_BJ = hr+8
  d = 0
  d_BJ = 0
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
  filename = "wrfout_d0"+domain+"_2019-12-"+day(d)+"_"+hour(hr)+":00:00"
  a        = addfile("../"+filename,"r")

;---Read several WRF variables at first time step
  it  = 0
  g   = 9.81
  u  = wrf_user_getvar(a,"ua",it)
  v  = wrf_user_getvar(a,"va",it)
  th = wrf_user_getvar(a,"th",it)
  z  = wrf_user_getvar(a,"z",it)
  hgt = wrf_user_getvar(a,"HGT",it)
  dim = dimsizes(u)
  dim_n = dim(0)
  dim_j = dim(1)
  dim_i = dim(2)
  level = new((/dim_j,dim_i/),integer)
  Ri = new((/dim_j,dim_i/),float)
  Rib0 = new((/dim_j,dim_i/),float)
  ablh0 = new((/dim_j,dim_i/),float)
;---Rib to get PBLH
  Rib_cr = 0.25
  do j = 0,dim_j-1
    do i = 0,dim_i-1
      do n = 1,dim_n-1
        Rib = (g/th(0,j,i))*(th(n,j,i)-th(0,j,i))*(z(n,j,i)-z(0,j,i))/((u(n,j,i)-u(0,j,i))^2+(v(n,j,i)-v(0,j,i))^2)
        if (Rib.ge.Rib_cr) then 
		  level(j,i) = n
		  break
		end if
	  end do
	end do
  end do
  
  do j = 0,dim_j-1
    do i = 0,dim_i-1
      z_mid = (z(level(j,i)-1,j,i)+z(level(j,i),j,i))/2
      z_min = tofloat(z(level(j,i)-1,j,i))
      z_max = tofloat(z(level(j,i),j,i))
      th_point = th(:,j,i)
      u_point = u(:,j,i)
      v_point = v(:,j,i)
      z_point = z(:,j,i)
      do while ((z_max-z_min).ge.2)
        u_mid = wrf_interp_1d(u_point,z_point,z_mid)
        v_mid = wrf_interp_1d(v_point,z_point,z_mid)
        th_mid = wrf_interp_1d(th_point,z_point,z_mid)
        Rib = (g/th(0,j,i))*(th_mid-th(0,j,i))*(z_mid-z(0,j,i))/((u_mid-u(0,j,i))^2+(v_mid-v(0,j,i))^2)
        if (Rib.ge.Rib_cr) then
          z_max = z_mid
          z_mid = (z_max+z_min)/2
        else
          z_min = z_mid
          z_mid = (z_max+z_min)/2
        end if
      end do
    ablh0(j,i) = (z_max+z_min)/2-hgt(j,i)
    end do
  end do
	    
;---Set common resources for all plots
  type = "png"
  wks = gsn_open_wks(type,"pblh_les_0.25/"+"pblh_d0"+domain+"_2019-12-"+day(d_BJ)+"_"+hour(hr_BJ)+":00:00")

; subtract one since we want to use it as an index in NCL
  x_start = 4
  x_end   = 131
  y_start = 4
  y_end   = 131

 ablh = new((/128,128/),"float")

 do j = 0,127
 do i = 0,127
 ablh(j,i) = ablh0(j+y_start,i+x_start)
 end do
 end do

  ablh@description = "PBL Height"
  ablh@units = "m"
;---Set common resources for all plots
  res                = True
  res@gsnFrame       = False
  res@gsnDraw        = False
  res@gsnLeftString  = ""
  res@gsnRightString = ""

;---Necessary for contours to be overlaid correctly on WRF projection
;  res@tfDoNDCOverlay        = True
  res@tfDoNDCOverlay        = "NDCViewport" ; can use this in NCL V6.5.0 or later
  
  ;---ABLH filled contour plot
  ablh_res                      = res
  ablh_res@cnFillOn             = True
  ablh_res@cnInfoLabelOn        = False
  ablh_res@cnLineLabelsOn       = False
  ablh_res@cnLinesOn            = False
  ablh_res@cnFillDrawOrder      = "PreDraw"
  ablh_res@lbLabelFontHeightF   = 0.01
  ablh_res@pmLabelBarOrthogonalPosF = -0.06
  ablh_res@pmLabelBarHeightF    = 0.15

  contour_ablh = gsn_csm_contour(wks,ablh,ablh_res)

;---Map plot
  map_res                      = True
  map_res@gsnFrame             = False
  map_res@gsnDraw              = False
  map_res@tiMainString         = "2019-12-"+day(d_BJ)+"_"+hour(hr_BJ)+":00:00"
  map_res@gsnRightString        = ablh@description + " (" + ablh@units + ")"
  ;map_res@gsnLeftString       = ""
  map_res@gsnStringFontHeightF = 0.015

  map_res@ZoomIn = True        ; set up map info for zoomed area
  map_res@Xstart = x_start
  map_res@Ystart = y_start
  map_res@Xend = x_end
  map_res@Yend = y_end

;---Set map resources based on projection on WRF output file
  map_res = wrf_map_resources(a,map_res)
  map_res@mpFillOn                    = False
  map_res@mpOutlineOn                 = True
  map_res@mpDataBaseVersion           = "MediumRes"
  map_res@mpDataSetName               = "Earth..4"
  map_res@mpOutlineDrawOrder          = "PostDraw"
  map_res@mpUSStateLineColor          = "Black"
  map_res@mpPerimLineColor            = "Black"
  map_res@mpNationalLineColor         = "Black"
  map_res@mpLimbLineColor             = "Black"
  map_res@mpGridLineColor             = "Black"
  map_res@mpGeophysicalLineColor      = "Black"
  map_res@mpOutlineBoundarySets      = "AllBoundaries"
  map_res@mpUSStateLineThicknessF     = 3.0
  map_res@mpNationalLineThicknessF    = 3.0
  map_res@mpGeophysicalLineThicknessF = 3.0
  map_res@mpProvincialLineThicknessF  = 2.0
  map_res@pmTickMarkDisplayMode       = "Always"
  
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
   if (domain.le.2) then
     dum8  = gsn_add_text(wks,map,"Baoding",115.50,38.70,txres)
   end if
     dum21 = gsn_add_text(wks,map,"Tuonan",115.12,38.93,txres)
   if (domain.eq.1) then
     dum26 = gsn_add_text(wks,map,"Renqiu",116.10,38.50,txres)
   else
     dum26 = gsn_add_text(wks,map,"Renqiu",116.10,38.60,txres)
   end if


;---Overlay plots on map and draw.
  overlay(map,contour_ablh)

  draw(map)   ; This will draw all overlaid plots and the map
  frame(wks)

hr=hr+1
hr_BJ=hr_BJ+1
end do
end
