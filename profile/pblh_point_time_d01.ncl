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
  ;do h=1,run_hour

  if hr_BJ.ge.24 then
  hr_BJ = 0
  d_BJ =d_BJ+1
  end if

  if hr.ge.24 then
  hr=0
  d=d+1
  end if
stnm = "baoding"
lat_o = 38.80
lon_o = 115.50
;stnm = "renqiu"
;lat_o=38.74
;lon_o=116.10
  domain=1
;---Open WRF output file
    filename = systemfunc ("ls "+"../"+"wrfout_d0"+domain+"_2019-12-"+"*")
    a        = addfiles (filename,"r")
  ;filename = "wrfout_d0"+domain+"_2019-12-"+day(d)+"_"+hour(hr)+":00:00"
  ;a        = addfile("../"+filename,"r")

loc = wrf_user_ll_to_xy(a,lon_o,lat_o,True)                
;print(loc)

ablh0 = wrf_user_getvar(a,"PBLH",-1)
;print(ablh0)
dim  = dimsizes(ablh0)
;print(dim)
all_n=dim(0) ;time
all_j=dim(1) ;south-north
all_i=dim(2) ;west-east

ablh  = ablh0(:,loc(1),loc(0))
;print(ablh)

;zi = reshape(ablh, (/2161,2/))

fnm="./pblh/"+stnm+"d0"+domain+"_PBLH_ysu.dat"
  fmtf    = "36f8.2"
  option  = True
  option@fout = fnm
  asciiwrite(fnm,sprintf("%9.3f",ablh))


;delete(filename)
;delete(a)
;delete(ablh0)
;delete(ablh)
;delete(zi)

end

