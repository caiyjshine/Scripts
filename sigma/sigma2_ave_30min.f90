program sigma_uvw
    real,ALLOCATABLE::sigma2(:,:,:)
    real,ALLOCATABLE::sigma(:,:)
    real,ALLOCATABLE::uvw(:,:,:,:)
    real,ALLOCATABLE::domain_ave(:,:,:)
    real,ALLOCATABLE::time_domain_ave(:,:)
    integer::i,j,k,n=0,spin_up,start_hour,end_hour,time,hour,levels
    character(len=2)::var
    character(len=2)::filename,hour_char
    write(*,*)"input the variable in capital"
    read(*,*)var
    write(*,*)"input the start hour"
    read(*,*)start_hour
    write(*,*)"input the end hour"
    read(*,*)end_hour
    write(*,*)"input (n) half-hours to spin-up"
    read(*,*)spin_up
    write(*,*)"input levels"
    read(*,*)levels
    
    allocate(sigma2(5,2,levels))
    allocate(sigma(10,levels))
    allocate(uvw(60,levels,79,79))
    allocate(domain_ave(2,30,levels))   
    allocate(time_domain_ave(2,levels))
    sigma2(:,:,:)=0.
    sigma(:,:)=0.
    uvw(:,:,:,:)=0.
    domain_ave(:,:,:)=0.
    time_domain_ave(:,:)=0.
    do hour=start_hour+spin_up/2,end_hour
        write(hour_char,"(i2)")hour
	    open(1,file=hour_char//"_"//var)
        write(*,*)"reading "//hour_char//"_"//var
        do level=1,levels
            do time=1,30
                do lat=1,79
                    read(1,*)(uvw(time,level,lat,lon),lon=1,79)
                    do lon=1,79 !sum the lon and then lat
                        domain_ave(1,time,level)=domain_ave(1,time,level)+uvw(time,level,lat,lon)
                    end do
                end do
                domain_ave(1,time,level)=domain_ave(1,time,level)/(79*79) !ave domain
            end do
            do time=31,60
                do lat=1,79
                    read(1,*)(uvw(time,level,lat,lon),lon=1,79)
                    do lon=1,79
                        domain_ave(2,time,level)=domain_ave(2,time,level)+uvw(time,level,lat,lon)
                    end do
                end do
                domain_ave(2,time,level)=domain_ave(2,time,level)/(79*79)
            end do
        end do
        close(1)
        do level=1,levels
            do time=1,30
                time_domain_ave(1,level)=time_domain_ave(1,level)+domain_ave(1,time,level)
            end do
            time_domain_ave(1,level)=time_domain_ave(1,level)/30
            do time=31,60
                time_domain_ave(2,level)=time_domain_ave(2,level)+domain_ave(2,time,level)
            end do
            time_domain_ave(2,level)=time_domain_ave(2,level)/30
        end do
        do level=1,levels
            do time=1,30
                do lon=1,79
                    do lat=1,79
                        sigma2(hour,1,level)=(uvw(time,level,lat,lon)-time_domain_ave(1,level))**2+sigma2(hour,1,level)
                    end do
                end do
            end do
            sigma2(hour,1,level)=sigma2(hour,1,level)/(30*79*79)
            do time=31,60
                do lon=1,79
                    do lat=1,79
                        sigma2(hour,2,level)=(uvw(time,level,lat,lon)-time_domain_ave(2,level))**2+sigma2(hour,2,level)
                    end do
                end do
            end do
            sigma2(hour,2,level)=sigma2(hour,2,level)/(30*79*79)
        end do   
    !!!!!!!!Remember ave=0!!!!!!!!
    domain_ave=0.
    time_domain_ave=0.
    uvw=0.
    end do

    do level=1,levels
        k=1
        do hour=start_hour+spin_up/2,end_hour
            sigma(k,level)=sigma2(hour,1,level)
            k=k+1
            sigma(k,level)=sigma2(hour,2,level)
            k=k+1
        end do
    end do
    
    open(2,file=var)
    do level=1,levels
        write(2,"(I2,1X,10(F11.8,1X))")level,(sigma(i,level),i=1,k-1) !remember how many list to output
    end do

    close(2)
DEALLOCATE(sigma2)
DEALLOCATE(sigma)
DEALLOCATE(uvw)
DEALLOCATE(domain_ave)
DEALLOCATE(time_domain_ave)
end program sigma_uvw