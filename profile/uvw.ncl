begin
hour = (/"00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"/)

  u_pro = new((/75,18/),float)
  v_pro = new((/75,18/),float)
  u_resultant_pro = new((/75,18/),float)

do h = 12,20
;---Open WRF output file for time = 0
  ;filename = systemfunc ("ls "+"../../"+"wrfout_d0"+domain+"_2019-12-"+"*")
  ;a        = addfiles (filename,"r")
  a = addfile("../../wrfout_d01_2019-12-07_"+hour(h)+":00:00.nc","r")

;---Read several WRF variables at first time step
  it_0 = 0

  u_0     = wrf_user_getvar(a,"ua",it_0)    ; 3D U at mass points
  v_0     = wrf_user_getvar(a,"va",it_0)    ; 3D V at mass points
  ;w_0     = wrf_user_getvar(a,"wa",it_0)    ; 3D W at mass points

  u_pro(:,2*(h-12)+1) = u_0(0:74,50,50)
  v_pro(:,2*(h-12)+1) = v_0(0:74,50,50)
  u_resultant_pro(:,2*(h-12)+1) = sqrt(u_0(0:74,50,50)^2+v_0(0:74,50,50)^2)
end do
do h = 11,19
;---Open WRF output file for time = 180
  ;filename = systemfunc ("ls "+"../"+"wrfout_d0"+domain+"_2019-12-"+"*")
  ;a        = addfiles (filename,"r")
  a = addfile("../../wrfout_d01_2019-12-07_"+hour(h)+":00:00.nc","r")

;---Read several WRF variables at first time step
  it_0 = 180

  u_180     = wrf_user_getvar(a,"ua",it_0)    ; 3D U at mass points
  v_180     = wrf_user_getvar(a,"va",it_0)    ; 3D V at mass points
  ;w_0     = wrf_user_getvar(a,"wa",it_0)    ; 3D W at mass points

  u_pro(:,2*(h-11)) = u_180(0:74,50,50)
  v_pro(:,2*(h-11)) = v_180(0:74,50,50)
  u_resultant_pro(:,2*(h-11)) = sqrt(u_180(0:74,50,50)^2+v_180(0:74,50,50)^2)
end do

opt_u = True
opt_u@fout = "u_profile"
write_matrix (u_pro, "18f7.3", opt_u)

opt_v = True
opt_v@fout = "v_profile"
write_matrix (v_pro, "18f7.3", opt_v)

opt_UU = True
opt_UU@fout = "u_resultant_profile"
write_matrix (u_resultant_pro, "18f7.3", opt_UU)
end

