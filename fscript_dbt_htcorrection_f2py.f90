	
	
	subroutine  dbt_ht_correction(dbt_10,sph_10,ws_10,rh_10,dbt1,dbt_ht1,sst1,airp1,rh1,ws1,ws_ht1,vessel1,vos1,wsind1)
	
	implicit none

!	integer, intent(in) :: rr
	character*2, intent(in) :: vessel1
	character*7, intent(in) :: vos1
	character, intent(in) :: wsind1
	double precision, intent(in) :: dbt1, dbt_ht1, sst1, airp1
	double precision, intent(in) :: rh1, ws1, ws_ht1
	double precision, intent(out) :: dbt_10, sph_10, ws_10
	double precision, intent(out) :: rh_10

	double precision k,g,gaama,default_ht,tol
	double precision sigma_n,sigma_nn,sigma_n_10,delta_sigma,S,L
	double precision f_sigma_n,g_sigma_n,h_sigma_n,X
	double precision delta_theta,delta_theta_10,dbt
	double precision delta_sph,delta_sph_10,ew,q,qs,airp
	double precision psiim,psiim_10,psiit,psiit_10,psiiq,psiiq_10
	double precision cdn,cd,cd10,ctn,ct,ct10,cen,ce10,cd_zdbt,ct_zdbt
	double precision diff,a,cd_ws,ws_zdbt,ws,ws_zdbt_o,ce_zdbt
	double precision sst,z_dbt,ev,wdir,z_ws,rh,ea,es
	

	double precision pi
	integer counter,nn,pos1,pos2,ij,sph_missing,ws_ht_notfound,length

	character inputfile*50,outputfile1*50,record*200,outputfile2*50,outputfile3*50
	character*10 :: word(35)
	character*6 :: formatt(35)

	integer y1,m1,d1,mins,lineno,ws_counter
	real lat1,lon1,wbt1,missing,h1
	real wdir1,shipspeed1,shipdir1,dbt_ht,pf_ht,sst_depth,ws_ht
	
	character callsign*10,wsind,sstind,source,obspf,cc*2,ww*2
	character cla,glc,gmc,ghc,vos*7,vessel*2,expofhyg*2,sstind_wmo*3


	k=0.4
	g=9.8
	gaama=0.01
	default_ht=18
	tol=0.0001
	pi=3.142
	missing=-999.9
	pf_ht=0
	ws_counter=0
	sph_missing=0
	ws_ht_notfound=0


	dbt_ht=dble(dbt_ht1)
	sst=dble(sst1)
	airp=dble(airp1)
	rh=dble(rh1)
	dbt=dble(dbt1)
	ws=dble(ws1)
	ws_ht=dble(ws_ht1)
	
	dbt_10 = -999.9
	sph_10 = -999.9
	ws_10 = -999.9
	rh_10 = -999.9
	
!	while ii <= rr

	if ((airp.ne.-9999.0).and.(sst.ne.-999.9).and.(ws.ne.-999.9).and.&
		(ws.ne.0.0).and.(airp.ge.960.0).and.(airp.le.1040.0).and.&
		(sst.ge.10).and.(sst.le.32).and.(dbt.ne.-999.9)&
		.and.(ws.gt.0).and.(rh.ge.50).and.(rh.le.100)) then
	
! finding out dbt measurement height
	
	if ((dbt_ht.ne.0.0).and.(dbt_ht.gt.0)) then
		z_dbt=dbt_ht
	elseif ((dbt_ht.eq.0.00).and.(pf_ht.eq.0.00)) then
		if ((vessel.eq.'CC') .or. (vessel.eq.'CS')) then
			z_dbt=28.8+1.0
		elseif (vessel.eq.'GC') then
			z_dbt=16.3+1.0
		elseif (vessel.eq.'LT') then
			z_dbt=22.1+1.0
		elseif (vessel.eq.'RV') then
			z_dbt=10.0+1.0
		elseif (vessel.eq.'BC') then
			z_dbt=21.3+1.0
		elseif (vessel.eq.'FV') then
			z_dbt=11.1+1.0
		elseif (vessel.eq.'RR') then
			z_dbt=22.6+1.0
		elseif (vessel.eq.'T') then
			z_dbt=10.1+1.0
		elseif (vessel.eq.'RF') then
			z_dbt=25.1+1.0
		elseif (vessel.eq.'RS') then
			z_dbt=20.0+1.0
		elseif (vessel.eq.'GT') then
			z_dbt=31.2+1.0
		elseif (vessel.eq.'O') then
			z_dbt=12.0+1.0
		elseif (vessel.eq.'TU') then
			z_dbt=15.9+1.0
		elseif (vessel.eq.'SV') then
			z_dbt=24.1+1.0
		elseif (vessel.eq.'MS') then
			z_dbt=22.2+1.0
		elseif (vessel.eq.'PV') then
			z_dbt=21.5+1.0
		elseif (vessel.eq.'SV') then
			z_dbt=24.1+1.0
		elseif (vessel.eq.'IC') then
			z_dbt=16.9+1.0
		elseif (vessel.eq.'LC') then
			z_dbt=22.4+1.0
		elseif (vessel.eq.'CG') then
			z_dbt=13.4+1.0
		elseif (vessel.eq.'SA') then
			z_dbt=8.1+1.0
		elseif (vessel.eq.'Y') then
			z_dbt=4.0+1.0	
		elseif ((vos.eq."SEL TRW") .or. (vos.eq."SUP TRW")) then
			z_dbt=10.1+1.0
		else
			z_dbt=default_ht
		endif
	elseif ((dbt_ht.eq.0.0).and.(pf_ht.gt.0.0)) then
		z_dbt=pf_ht+1	! r12
	else
		z_dbt=default_ht
	endif
	write(*,*)'am here'
! calculating specific humidity
	
	es = 6.1121*(1.0007+3.46*0.000001*airp)*exp((17.5*dbt)/(240.97+dbt))
	ea=0.01*rh*es
	q=(0.622*ea)/(airp-(0.378*ea))

	

! finding out wind speed measurement height

	if ((wsind.eq.'0').or.(wsind.eq.'  ').or.(wsind.eq.'')) then
		z_ws=10.0
		!ws10=ws
		goto 103
	elseif ((wsind.eq.'3').or.(wsind.eq.'2').or.(wsind.eq.'6')) then
		z_ws=10
		!ws10=ws
		goto 103
	elseif ((wsind.eq.'1').or.(wsind.eq.'7'))then
		goto 101
	elseif ((wsind.eq.'4')) then
		!ws=ws
		goto 101
	endif

101	if ( (ws_ht.gt.0)) then
		z_ws=ws_ht
	elseif ((vos.eq."SEL TRW") .or. (vos.eq."SUP TRW")) then
		z_ws=15.0
	else
		ws_ht_notfound=1
	endif
		
	if (ws_ht_notfound.eq.1) then
		goto 102
	else
		goto 103
	endif

	

102	if ((vessel.eq.'CC') .or. (vessel.eq.'CS')) then
		z_ws=37.4
	elseif (vessel.eq.'GC') then
		z_ws=22.9
	elseif (vessel.eq.'LT') then
		z_ws=30.8
	elseif (vessel.eq.'RV') then
		z_ws=22.8
	elseif (vessel.eq.'BC') then
		z_ws=30.8
	elseif (vessel.eq.'FV') then
		z_ws=19.0
	elseif (vessel.eq.'RR') then
		z_ws=30.9
	elseif (vessel.eq.'T') then
		z_ws=15.0	
	elseif (vessel.eq.'RF') then
		z_ws=34.3
	elseif (vessel.eq.'RS') then
		z_ws=28
	elseif (vessel.eq.'GT') then
		z_ws=42.4
	elseif (vessel.eq.'O') then
		z_ws=21.2
	elseif (vessel.eq.'TU') then
		z_ws=19.5
	elseif (vessel.eq.'SV') then
		z_ws=52.9
	elseif (vessel.eq.'MS') then
		z_ws=33.7
	elseif (vessel.eq.'PV') then
		z_ws=32
	elseif (vessel.eq.'SV') then
		z_ws=22.1
	elseif (vessel.eq.'IC') then
		z_ws=32.9
	elseif (vessel.eq.'LC') then
		z_ws=34.8
	elseif (vessel.eq.'CG') then
		z_ws=23.2
	elseif (vessel.eq.'SA') then
		z_ws=36.1
	elseif (vessel.eq.'Y') then
		z_ws=27.1
	else
		z_ws=20.0
		
	endif

! converting ws at z_ws to a height of dbt measurement
103	continue	
! log-layer correction factor
	a=(log10(z_ws/z_dbt))/0.4			!here we are converting ws at 10m ht to a height if dbt_ht (z)
! tolerance for iteration [m/s]
	tol=0.001            

	ws_zdbt_o=0

	cd_zdbt=(1.15)*(0.001)

	ws_zdbt=(ws/(1+a*sqrt(cd_zdbt)))

	write(50,*)a,z_ws,z_dbt	

	diff=abs(ws_zdbt-ws_zdbt_o)

	do
		if (diff.le.tol) goto 104
	  	ws_zdbt_o=ws_zdbt									
		if (ws_zdbt_o .lt. 10.15385) cd_zdbt=(1.15)*(0.001)
	  	if (ws_zdbt_o .gt. 10.15385) cd_zdbt=(4.9*(0.0001)+6.5*(0.00001)*ws_zdbt_o)    			! next iteration
 		ws_zdbt=ws/(1+a*sqrt(cd_zdbt))									! keep going until iteration converges	
		diff=abs(ws_zdbt-ws_zdbt_o)
	enddo
	
! calculating delta_theta
104	delta_theta=(273.15+sst)-(273.15+dbt)-(gaama*z_dbt)
	
! calculating delta_sph
	ew=6.1121*(1.0007+3.46*0.000001*airp)*exp((17.5*sst)/(240.97+sst))
	qs=0.62197*(ew/(airp-(0.378*ew)))
	delta_sph=(0.98*qs)-q
	write(*,*)sst
	S=-k*((g*z_dbt*delta_theta)/(ws_zdbt**2*(273.15+dbt)))*(1+((273.15+dbt)**2*&
		(delta_sph/delta_theta)*1.72*0.000001))
	! neutral drag coeffiecient		
	if (ws_zdbt.le.1) then
		cdn=2.18*0.001
	elseif ((ws_zdbt.gt.1).and.(ws_zdbt.lt.3)) then
		cdn=(0.62+(1.56/ws_zdbt))*0.001
	elseif ((ws_zdbt.ge.3).and.(ws_zdbt.lt.10)) then
		cdn=1.14*0.001
	elseif (ws_zdbt.ge.10) then
		cdn=(0.49+(0.065*ws_zdbt))*0.001
	endif
	! neutarl transfer coefficeint for temperature
	if (S.lt.0) then
		ctn=1.2*0.001
	elseif (S.ge.0) then
		ctn=0.75*0.001
	endif
	! neutral transfer coefficient for moisture
	cen=1.2*0.001
	
	
	
	delta_sigma=tol+1
	counter=1
	do
105	if(delta_sigma > tol) then
		!if(lineno.gt.44000) write(50,*)sigma_n !"iteration num:"
		!write(50,*)counter
		!write(*,*)q
		if (counter.eq.1) then
			sigma_n=(ctn/(cdn**1.5))*S
			delta_sigma=tol+1
			counter=counter+1
			goto 105
		endif
		!write(50,*)sigma_n
		if (sigma_n>0) then
			psiim=-(7.0)*sigma_n
			psiit=-(7.0)*sigma_n
			psiiq=-(7.0)*sigma_n
		elseif (sigma_n<0) then
			X=(1-(16*sigma_n))**0.25
			psiim=2*log((1+X)/2)+log((1+X**2)/2)-2*atan(X)+(pi/2)
			psiit=2*log((1+X**2)/2)
			psiiq=2*log((1+X**2)/2)
		endif
		
		f_sigma_n=(1/(1-(sqrt(cdn)*psiim)/k)**2)
		cd=cdn*f_sigma_n
		g_sigma_n=f_sigma_n/(1-(ctn*psiit)/(sqrt(cdn)*k))
		ct=ctn*g_sigma_n
		sigma_nn=(ct/(cd**1.5))*S
		delta_sigma=abs(sigma_nn-sigma_n)
		
		counter=counter+1
!		write(50,*)dbt1,dpt1,q,S,sigma_n,X,g_sigma_n,ct,cd,sigma_nn,delta_sph,airp,delta_theta
		sigma_n=sigma_nn
		goto 105
	else
		!write(*,*)'iterations over'
		if (sigma_n>0) then
			psiim=-(7.0)*sigma_n
			psiit=-(7.0)*sigma_n
			psiiq=-(7.0)*sigma_n
		elseif (sigma_n<0) then
			X=(1-16*sigma_n)**0.25
			psiim=2*log((1+X)/2)+log((1+X**2)/2)-2*atan(X)+(pi/2)
			psiit=2*log((1+X**2)/2)
			psiiq=2*log((1+X**2)/2)
		endif		
		f_sigma_n=(1/(1-(sqrt(cdn)*psiim)/k)**2)
		cd_zdbt=cdn*f_sigma_n
		g_sigma_n=f_sigma_n/(1-(ctn*psiit)/(sqrt(cdn)*k))
		ct_zdbt=ctn*g_sigma_n
		h_sigma_n=f_sigma_n/(1-(cen*psiit)/(sqrt(cdn)*k))
		ce_zdbt=cen*h_sigma_n
! finding monin-obhukov length
		L=z_dbt/sigma_n		
! finding temperature at 10 m height
		sigma_n_10=10/L
		if (sigma_n_10>0) then
			psiim_10=-7*sigma_n_10
			psiit_10=-7*sigma_n_10
			psiiq_10=-7*sigma_n_10
		elseif (sigma_n_10<0) then
			X=(1-16*sigma_n_10)**0.25
			psiim_10=2*log((1+X)/2)+log((1+(X**2))/2)-2*atan(X)+(pi/2)
			psiit_10=2*log((1+(X**2))/2)
			psiiq_10=2*log((1+(X**2))/2)
		endif
		cd10=cdn/(1+(cdn/k)*(-psiim_10))**2
		ct10=(ctn*sqrt(cd10/cdn))/(1+(ctn/(k*sqrt(cdn))*(-psiit_10)))
		ce10=(cen*sqrt(cd10/cdn))/(1+(cen/(k*sqrt(cdn))*(-psiiq_10)))
		delta_theta_10=delta_theta/(1+(ct10/sqrt(cd10))*(log(z_dbt/10)-psiit+psiit_10))
		delta_sph_10=delta_sph/(1+(ce10/sqrt(cd10))*(log(z_dbt/10)-psiiq+psiiq_10))
		goto 106
	endif
	enddo
106	a=(log10(z_dbt/10))/0.4
	ws_10=(ws_zdbt/(1+a*sqrt(cdn)))
!	ws_10=ws_zdbt/(1+(sqrt(cd10)/k)*(log(z_dbt/10)-psiim+psiim_10))
	dbt_10=sst-delta_theta_10-(gaama*10)
!	if (sph_missing.ne.1) then
		sph_10=0.98*qs-delta_sph_10
		!	# calculating rh10
		ea=(airp*sph_10)/(0.622+(0.378*sph_10))
		rh_10=ea/(0.01*10**(((0.7859+0.03477*dbt_10)/(1.0+0.00412*dbt_10))+2))
!	endif
	

	
	
	endif
	
	return 
	end subroutine


