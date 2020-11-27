begin

stnm = "baoding"
lat_o = 38.80
lon_o = 115.50
;stnm = "renqiu"
;lat_o=38.74
;lon_o=116.10
  domain=3
;---Open WRF output file
  filename = systemfunc ("ls "+"../"+"wrfout_d0"+domain+"_2019-12-"+"*")
  a        = addfiles (filename,"r")
  ;a = addfile("../wrfout_d05_2019-12-12_08:00:00.nc","r")
  loc = wrf_user_ll_to_xy(a,lon_o,lat_o,True)
  g   = 9.81
  u  = wrf_user_getvar(a,"ua",-1)
  v  = wrf_user_getvar(a,"va",-1)
  th = wrf_user_getvar(a,"th",-1)
  z  = wrf_user_getvar(a,"z",-1)
  dim = dimsizes(u)
  dim_t = dim(0)
  dim_n = dim(1)
  dim_j = dim(2)
  dim_i = dim(3)
  ablh = new((/dim_t-1/),float)
  ;ablh = new((/60/),float)
;print(th(2160,0,loc(1),loc(0)))
print(dim)
;---Rib to get PBLH
do time=1,dim_t-1
  Rib_cr = 0.25
      do n = 1,dim_n-1
        Rib = (g/th(time,0,loc(1),loc(0)))*(th(time,n,loc(1),loc(0))-th(time,0,loc(1),loc(0)))*(z(time,n,loc(1),loc(0))-z(time,0,loc(1),loc(0)))\
/((u(time,n,loc(1),loc(0))-u(time,0,loc(1),loc(0)))^2+(v(time,n,loc(1),loc(0))-v(time,0,loc(1),loc(0)))^2)
        if (Rib.ge.Rib_cr) then
          level = n
          Ri = Rib
          break
        end if
      end do
  ;print(level)
  ;print(Ri)
  ;print(Rib0)

      z_mid = (z(time,level-1,loc(1),loc(0))+z(time,level,loc(1),loc(0)))/2
      z_min = tofloat(z(time,level-1,loc(1),loc(0)))
      z_max = tofloat(z(time,level,loc(1),loc(0)))
      th_point = th(time,:,loc(1),loc(0))
      u_point = u(time,:,loc(1),loc(0))
      v_point = v(time,:,loc(1),loc(0))
      z_point = z(time,:,loc(1),loc(0))
      do while ((z_max-z_min).ge.2)
        u_mid = wrf_interp_1d(u_point,z_point,z_mid)
        v_mid = wrf_interp_1d(v_point,z_point,z_mid)
        th_mid = wrf_interp_1d(th_point,z_point,z_mid)
        Rib = (g/th(time,0,loc(1),loc(0)))*(th_mid-th(time,0,loc(1),loc(0)))*(z_mid-z(time,0,loc(1),loc(0)))\
/((u_mid-u(time,0,loc(1),loc(0)))^2+(v_mid-v(time,0,loc(1),loc(0)))^2)
        if (Rib.ge.Rib_cr) then
          z_max = z_mid
          z_mid = (z_max+z_min)/2
        else
          z_min = z_mid
          z_mid = (z_max+z_min)/2
        end if
      end do
    ablh(time-1) = (z_max+z_min)/2
end do

fnm="./pblh/"+stnm+"d0"+domain+"_PBLH.dat"
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
