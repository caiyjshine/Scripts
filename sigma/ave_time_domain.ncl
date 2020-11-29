begin
stnm = "test"
hour = (/"00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"/)
  domain=1
do h = 11,16
;---Open WRF output file
  ;filename = systemfunc ("ls "+"../"+"wrfout_d0"+domain+"_2019-12-"+"*")
  ;a        = addfiles (filename,"r")
  a = addfile("../../wrfout_d01_2019-12-07_"+hour(h)+":00:00.nc","r")
  ua  = wrf_user_getvar(a,"ua",-1)
  va  = wrf_user_getvar(a,"va",-1)
  wa  = wrf_user_getvar(a,"wa",-1)
  levels = 55
  u = new((/levels*60*79,79/),float)
  v = new((/levels*60*79,79/),float)
  w = new((/levels*60*79,79/),float)
  k = 0

do level = 0,levels-1
  do time = 0,59
    do j = 10,88
      u(k,:) = ua(time*6,level,j,10:88)
      v(k,:) = va(time*6,level,j,10:88)
      w(k,:) = wa(time*6,level,j,10:88)
      k = k+1
    end do
  end do
end do

    fmtf = "79f6.2"
    u_opt = True
    u_opt@fout = h+"_UU"
    write_matrix(u(:,:),fmtf,u_opt)
    v_opt = True
    v_opt@fout = h+"_VV"
    write_matrix(v(:,:),fmtf,v_opt)
    w_opt = True
    w_opt@fout = h+"_WW"
    write_matrix(w(:,:),fmtf,w_opt)
delete(ua)
delete(va)
delete(wa)
delete(u)
delete(v)
delete(w)
end do
end
