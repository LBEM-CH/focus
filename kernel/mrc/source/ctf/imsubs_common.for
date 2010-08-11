C****************************************************************
C***	imsubs_common.for
C****************************************************************
	integer*4	max_units
	parameter	(max_units = 5)
	character*4     cstamp/'MAP'/
C***
	integer*4	karch(max_units)
	integer*4	labls(20,10,max_units)
	integer*4	lstream(12)
	integer*4	mapcrs(3,max_units)
	integer*4	mode(max_units)
	integer*4	nb(max_units)
	integer*4	nbsym(max_units)
	integer*4	ncrs(3,max_units)
	integer*4	ncrst(3,max_units)	
	integer*4	nlab(max_units)
	integer*4	nspg(max_units)
	integer*4	nxyz(3,max_units)
C***
	logical		flag(max_units)
	logical		old(max_units)
	logical		nocon(max_units)
	logical		unknown(max_units)
C***
	real*4		cel(6,max_units)
	real*4		denmmm(3,max_units)
	real*4          rstamp
	real*4		stuff(25,max_units)
	real*4		origin(3,max_units)
	real*4		stamp(2,max_units)
	real*4		rms(max_units)
C***
	real*8		dval
	real*8		dtot
C***
        common/imcom/ lstream,nbhdr,nbw,nbw3,nb,nbl,ncrs,mode,
     .          ncrst,nxyz,cel,mapcrs,denmmm,nspg,nbsym,stuff,
     .          origin,stamp,rms,nlab,labls,numopen,karch,
     .          nocon,flag,old,unknown
C***
        save    imcom
C***
	equivalence      (cstamp,rstamp)
