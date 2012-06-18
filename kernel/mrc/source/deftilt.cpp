/***************************************************************************************
****************************************************************************************
deftilt.C:
==========
Uses vGeometry.h (vs 2.0).
Use "CC -ansi deftilt.C -lm" to compile. vGeometry.h has to be in the same directory.
Purpose: Calculation of tiltangle from defocus values of 4 testimages in the corner.
The testimages are arranged as follows:
-------------
|P2       P3 |
|            |
|            |
|     M      |
|            |
|            |
|P1        P4|
-------------
All defocus values are given in Angstrom, underfocus is positive!
The coordinate system originates in P1 with 0 defocus.

Description of Parameters:
--------------------------
- D1,D2,D3,D4: Defocus values of testimages
- longsize: the whole sidelength of the original image the testimages are taken from
- small size: sidelength of the four testimages
- psize: Pixelsize in Angstroem
- mag: Magnification
- u1,u2,v1,v2: Coordinates of lattice vectors

Calculation of angles:       
---------------------------     
All angles of all four possible planes are calculated and a mean value is given back.
determination of the sign of the angles:
1. Case: tiltaxis is not parallel to Y axis:
   If D1 - D2 < 0 -> pos tiltangle
2. Case: tiltaxis is parallel to Y axis:
   If D4 - D1 < 0 -> pos tiltangle

All angles are positive if the vectors build a right-handed system, this mean 
the first vector has to be turned counter-clockwise.
Angles calculated:
- Angle between h vector and x-axis: lhxAngle
- Angle between x-axis and tiltaxis: TLTAXIS
- Angle between tiltaxis and h-latticevector in direction of hand: TLTAXA

History:
--------
29. 11. 1999: TB
28.3.2000: TB, HS  	angle definitions
04.09.2001: HS  	Sign of tiltangle output
15.04.2002: HS  	Debug.
16.04.2002: AS  	additional output of TLTANG,TLTAXIS, TLTAXA into tmp-file,debug
****************************************************************************************
****************************************************************************************/
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vGeometry.h"
/******************************************************************************************
Functions
*******************************************************************************************/   
inline double rad2absgrad(double a)
//converts grad to rad, always positive angles
{
	double b=(a/3.1415926535897932)*180.0;
	if (b < 0){b*=-1;};
	return b;
};

inline double rad2grad(double a)
//converts grad to grad, sign is kept
{return (a/3.1415926535897932)*180.0;};

/******************************************************************************************
Start of main program
*******************************************************************************************/ 
main()
{
	cout <<"Program to calculate tiltaxis and tiltangle from different defocus values" << endl;
 	cout <<"-------------------------------------------------------------------------" << endl;
	/************************
 	* Input of Parameters: 
	************************/
        int irunmode,i;
	double longsize, smallsize,k,D1,D2,D3,D4,oldTLTANG,locTLTANG,psize,mag,oldTLTAXA;
        double oldTLTAXIS,TLTAXI1,TLTAXI2,tmp1,tmp2,tmp4,tmp5,tmp7,TAXA;
        double DENOM,ANGA,ANGB,ANGDIF,ABSANGDIF,HAND,TLTNORM,ANGACOMP,AISABOVE;
        double SIGNTLTAXA,TANGL;
	double u1,u2,v1,v2;
	char vectors[80],name[80],nam2[80],nam3[80],nam4[80],nam5[80],nam6[80],nam7[80],nam8[80],s[80];
	cout << "Runmode: ";
 	cin >> irunmode;
        cout << irunmode << endl;
	cout << "Name for outputfile: ";
 	cin >> name;
        cout << name << endl;
 	cout << "Sidelength of whole image: ";
 	cin >> longsize; //Side-length between two Testimages
        cout << longsize << endl;
	cout << "Sidelength of test images: ";
 	cin >> smallsize;
        cout << smallsize << endl;
	k=longsize - smallsize;
	cout << "Size per pixel: ";
 	cin >> psize;
        cout << psize << endl;
	cout << "Magnification: ";
 	cin >> mag;
        cout << mag << endl;
	k*=(psize*10000/mag);
	cout << "Coordinates of u,v vectors: ";
	cin >> vectors;
        cout << vectors << endl;
	int z=sscanf(vectors,"%lf,%lf,%lf,%lf",&u1,&u2,&v1,&v2);
	if (z != 4 ){
		cerr <<"Wrong input for u, v vectors. \n Example: 12.3,5,3.1,6 \n";
		exit(-1);};
	cout << "Old tilt angle: TLTANG: ";
	cin >> oldTLTANG; 
        cout << oldTLTANG << endl;
	cout << "Local tilt angle: locTLTANG: ";
	cin >> locTLTANG; 
        cout << locTLTANG << endl;
	cout << "Old angle X-Axis -> Tilt Axis: TLTAXIS: ";
	cin >> oldTLTAXIS;  // tltaxis is angle X-Axis to Tilt Axis
        cout << oldTLTAXIS << endl;
	cout << "Old angle Tilt Axis -> A Axis: TLTAXA: ";
	cin >> oldTLTAXA;  // tltaxis is angle X-Axis to Tilt Axis
        cout << oldTLTAXA << endl;
 	cout << "Defocus of bottom left testimage: ";
 	cin >> D1;
        cout << D1 << endl;
 	cout << "Defocus of top left testimage: ";
 	cin >> D2;
        cout << D2 << endl;
 	cout << "Defocus of top right testimage: ";
 	cin >> D3;
        cout << D3 << endl;
 	cout << "Defocus of bottom right testimage: ";
 	cin >> D4;
        cout << D4 << endl;
 	cout << "TLTAXIS first version from emtilt: ";
 	cin >> TLTAXI1;
        cout << TLTAXI1 << endl;
 	cout << "TLTAXIS second version from emtilt: ";
 	cin >> TLTAXI2;
        cout << TLTAXI2 << endl;
	cout << "Name for emtilt decision file: ";
 	cin >> nam2;
        cout << nam2 << endl;
	cout << "Name for sign of tiltangle file: ";
 	cin >> nam3;
        cout << nam3 << endl;
	cout << "Name for TLTAXIS distance: ";
 	cin >> nam4;
        cout << nam4 << endl;
	cout << "Name for TLTANG distance: ";
 	cin >> nam5;
        cout << nam5 << endl;
	cout << "Name for tilt angles file: ";
 	cin >> nam6;
        cout << nam6 << endl;
	cout << "Name for local TLTAXIS distance: ";
 	cin >> nam7;
        cout << nam7 << endl;
	cout << "Name for mean defocus value: ";
 	cin >> nam8;
        cout << nam8 << endl;
        cout << endl << endl;
/*******************************************************************
* Creating of outputfile and commenting important values on screen
*******************************************************************/
	double M=(D1+D3+D2+D4)/4; //Mittler Defocus in der Mitte des Bildes
 	//Defocus in center of whole image
        ofstream rou8( nam8, ios::out); //creates new outfile
        if (!nam8){
                cerr << "ERROR: Not able to create output file ";
                cerr << nam8 << ". File already exists \n";
                exit(-1);};
        sprintf(s,"%9.1f %9.1f 0.00",M,M);
        rou8 << s << endl;

 	cout <<"--------------------------------------------------------" << endl;
 	cout << "Defocus in center of whole image: " << M << endl;
	cout <<"--------------------------------------------------------" << endl;
	ofstream rout( name, ios::out); //creates new outfile
	if (!name){
		cerr << "ERROR: Not able to create output file ";
		cerr << name << ". File already exists \n";
		exit(-1);};
	cout << "**************************************************" << endl;
	cout << "Calculation of tilt parameters from defocus values" << endl;
	cout << "**************************************************" << endl << endl << endl;
	cout << "Using following values for the calculations:" << endl;
	cout << "============================================:" << endl;
	cout << "- Sidelength between centers of testimages: " << k << endl;
	cout << "- Defocus values of testimage P1,2,3,4: " << D1 << ", " << D2 << ", " << D3 << ", " << D4 << endl;
	cout << "- Mean defocus value: " << M << endl;
 /*****************************
 * Construction of Vectors:
 ******************************/
 	Vector tmp(0,0,0); //Vector for various purposes
 	Vector tmpa(0,0,0); //Vector for various purposes
 	Vector tmpb(0,0,0); //Vector for various purposes
	//Vectors of tilted plane:
 	Vector r1(0.0,0.0,D1); //Direction: Tiltscanaxis
 	Vector r2(0.0,k  ,D2);
 	Vector r3(k  ,k  ,D3);
 	Vector r4(k  ,0.0,D4);
	//vectors to horizontal plane:
 	Vector rm1(0.0,0.0,M); //Direction: Tiltscanaxis
 	Vector rm2(0.0,k  ,M);
	Vector rm3(k  ,k  ,M);
 	Vector rm4(k  ,0.0,M);
	//lattice vectors: pointvector - rm1
	Vector lh(u1,u2,0.0);
	Vector lk(v1,v2,0.0);
	//Construction of horizontal plane (normal vector is looking up!):
	Plane horizontal(rm1,rm2,rm4);
	//Construction of planes (normalvectors are looking up!):
	Plane tilt124(r1,r2,r4);
	Plane tilt231(r2,r3,r1);
	Plane tilt342(r3,r4,r2);
	Plane tilt413(r4,r1,r3);
	//arrays for results:
	double rsx[4], rsy[4], tiltangle[4]; //Crossing point with x or y axis, results tiltangle
	int control[4]={0,0,0,0}; /* Control arrays for special cases: 
                                   0: all normal;
                                   1: Plane horizontal;
                                   2: Tiltaxis parallel to x axis, 
                                   3: Tiltaxis parallel to y-axis */
	Vector sx(0,0,0), sy(0,0,0); //Vectors to the crossing points (temporary used)
/****************************************
Calculations of the different planes
****************************************/
	cout << endl << "Warnings and comments during Calculations:" << endl;
	cout << "==========================================:" << endl;
//_____________________________________________
//Erste Ebene tilt 124: Slot 0
//_____________________________________________
	if (tilt124.pParallel(horizontal) == true)
		{
		cout << "- P124 is parallel to horizontal Plane" << endl;
		sx.x=0;sy.y=0;
		control[0]=1;
		tiltangle[0]=rad2absgrad(horizontal.pAngle(tilt124));
		}
	else 
       	{tmp=rm4-rm1;
	if (tilt124.pvParallel(tmp) == true)
		{
		cout << "- P124 is parallel to x-axis" << endl;
		sx.x=0;
		sy=tilt124.pCrosspoint(rm1, rm2-rm1);
		control[0]=2;
		tiltangle[0]=rad2absgrad(horizontal.pAngle(tilt124));
		}
	else {
	tmp=rm2-rm1;
	if (tilt124.pvParallel(tmp) == true)
		{
		cout << "- WARNING: P124 is parallel to y-axis" << endl;
		sx=tilt124.pCrosspoint(rm1, rm4-rm1);
		sy.y=0;
		control[0]=3;
		tiltangle[0]=rad2absgrad(horizontal.pAngle(tilt124));
		}	
	else	{
		sx=tilt124.pCrosspoint(rm1, rm4-rm1);
		sy=tilt124.pCrosspoint(rm1, rm2-rm1);
		tiltangle[0]=rad2absgrad(horizontal.pAngle(tilt124));
		}}} 
        if(sx.x > 0 )
        {
		rsx[0]=sx.x;
		rsy[0]=sy.y;
        } else
        {
		rsx[0]=-sx.x;
		rsy[0]=-sy.y;
	}
//_____________________________________________
//Second plane tilt 231: Slot 1
//_____________________________________________
	if (tilt231.pParallel(horizontal) == true)
		{
		cout << "- P231 is parallel to horizontal Plane" << endl;
		sx.x=0;sy.y=0;
		control[1]=1;
		tiltangle[1]=rad2absgrad(horizontal.pAngle(tilt231));
		}
	else 
       	{tmp=rm4-rm1;
	if (tilt231.pvParallel(tmp) == true)
		{
		cout << "- P231 is parallel to x-axis" << endl;
		sx.x=0;
		sy=tilt231.pCrosspoint(rm1, rm2-rm1);
		control[1]=2;
		tiltangle[1]=rad2absgrad(horizontal.pAngle(tilt231));
		}
	else {
	tmp=rm2-rm1;
	if (tilt231.pvParallel(tmp) == true)
		{
		cout << "- WARNING: P231 is parallel to y-axis" << endl;
		sx=tilt124.pCrosspoint(rm1, rm4-rm1);
		sy.y=0;
		control[1]=3;
		tiltangle[1]=rad2absgrad(horizontal.pAngle(tilt231));
		}	
	else	{
		sx=tilt231.pCrosspoint(rm1, rm4-rm1);
		sy=tilt231.pCrosspoint(rm1, rm2-rm1);
		tiltangle[1]=rad2absgrad(horizontal.pAngle(tilt231));
		}}} 
        if(sx.x > 0 )
        {
		rsx[1]=sx.x;
		rsy[1]=sy.y;
        } else
        {
		rsx[1]=-sx.x;
		rsy[1]=-sy.y;
	}
//_____________________________________________
//Dritte Ebene tilt 342: Slot 2
//_____________________________________________
	if (tilt342.pParallel(horizontal) == true)
		{
		cout << "- P342 is parallel to horizontal Plane" << endl;
		sx.x=0;sy.y=0;
		control[2]=1;
		tiltangle[2]=rad2absgrad(horizontal.pAngle(tilt342));
		}
	else 
       	{tmp=rm4-rm1;
	if (tilt342.pvParallel(tmp) == true)
		{
		cout << "- P342 is parallel to x-axis" << endl;
		sx.x=0;
		sy=tilt231.pCrosspoint(rm1, rm2-rm1);
		control[2]=2;
		// comment
		tiltangle[2]=rad2absgrad(horizontal.pAngle(tilt342));
		}
	else {
	tmp=rm2-rm1;
	if (tilt342.pvParallel(tmp) == true)
		{
		cout << "- WARNING: P342 is parallel to y-axis" << endl;
		sx=tilt342.pCrosspoint(rm1, rm4-rm1);
		sy.y=0;
		control[2]=3;
		tiltangle[2]=rad2absgrad(horizontal.pAngle(tilt342));
		}	
	else	{
		sx=tilt342.pCrosspoint(rm1, rm4-rm1);
		sy=tilt342.pCrosspoint(rm1, rm2-rm1);
		tiltangle[2]=rad2absgrad(horizontal.pAngle(tilt342));
		}}} 
        if(sx.x > 0 )
        {
		rsx[2]=sx.x;
		rsy[2]=sy.y;
        } else
        {
		rsx[2]=-sx.x;
		rsy[2]=-sy.y;
	}
//_____________________________________________
//fourth plane tilt 413 Slot 3
//_____________________________________________
	if (tilt413.pParallel(horizontal) == true)
		{
		cout << "- P413 is parallel to horizontal Plane" << endl;
		sx.x=0;sy.y=0;
		control[3]=1;
		tiltangle[3]=rad2absgrad(horizontal.pAngle(tilt413));
		}
	else 
       	{tmp=rm4-rm1;
	if (tilt413.pvParallel(tmp) == true)
		{
		cout << "- P413 is parallel to x-axis" << endl;
		sx.x=0;
		sy=tilt413.pCrosspoint(rm1, rm2-rm1);
		control[3]=2;
		tiltangle[3]=rad2absgrad(horizontal.pAngle(tilt413));
		}
	else {
	tmp=rm2-rm1;
	if (tilt413.pvParallel(tmp) == true)
		{
		cout << "- WARNING: P413 is parallel to y-axis" << endl;
		sx=tilt413.pCrosspoint(rm1, rm4-rm1);
		sy.y=0;
		control[3]=3;
		tiltangle[3]=rad2absgrad(horizontal.pAngle(tilt413));
		}	
	else	{
		sx=tilt413.pCrosspoint(rm1, rm4-rm1);
		sy=tilt413.pCrosspoint(rm1, rm2-rm1);
                tiltangle[3]=rad2absgrad(horizontal.pAngle(tilt413));
		}}} 
        if(sx.x > 0 )
        {
		rsx[3]=sx.x;
		rsy[3]=sy.y;
        } else
        {
		rsx[3]=-sx.x;
		rsy[3]=-sy.y;
	}
/***********************************************
Commenting of results as terminal output
************************************************/
	bool flagy=false; //Flag if tiltaxis is parallel to y-axis
	const int count=4;
	/*
	 * Berechnung des Mittelwertes und des x-Achsen Mittelwertes:
	 */ 
	double xmean=0.0;int j=0;
	cout << "Crossing x at: " << endl;
	for (i = 0; i < count;i++)
	{
		if ((control[i]==0) || (control[i]==3)) //Calculating mean only if there 
			{ xmean+=rsx[i];j++;
			cout << "   x" << i << ": " << rsx[i] <<endl; }
		}
	if ( j != 0 )
	{
	xmean/=j; //Mittelwert
	cout << "-------------------------- \n mean: ";
	cout << xmean << " (" << j << " mesurements)" << endl;
	//Berechnung des mittleren Fehlers des Mittelwertes:
	double xSDV=0.0;j=0;
	for (i = 0; i < count;i++)
		{
		if ((control[i]==0) || (control[i]==3)) //Calculating mean only if there 
			{ xSDV+=(rsx[i]-xmean)*(rsx[i]-xmean);j++;}
		}
	xSDV=sqrt(xSDV/(j*(j-1)));
	cout << " mean error of mean: " << xSDV << endl;
	}
	else
	{cout << "No values!" << endl;
	}
	cout << "==========================" << endl;
	/*
	 * Berechnung des Mittelwertes und des y-Achsen Mittelwertes:
	 */ 
	double ymean=0.0;j=0;
	cout << "Crossing y at: " << endl;
	for (i = 0; i < count;i++)
	{
		if ((control[i]==0) || (control[i]==2)) //Calculating mean only if there 
			{ ymean+=rsy[i];j++;
			cout << "   y" << i << ": " << rsy[i] <<endl; }
		}
	if ( j != 0 )
	{
	ymean/=j; //Mittelwert
	cout << "-------------------------- \n mean: ";
	cout << ymean << " (" << j << " mesurements)" << endl;
	//Berechnung des mittleren Fehlers des Mittelwertes:
	double ySDV=0.0;j=0;
	for (i = 0; i < count;i++)
		{
		if ((control[i]==0) || (control[i]==2)) //Calculating mean only if there 
			{ ySDV+=(rsy[i]-ymean)*(rsy[i]-ymean);j++;}
		}
	ySDV=sqrt(ySDV/(j*(j-1)));
	cout << " mean error of mean: " << ySDV << endl;
	}
	else
	{cout << "No values!" << endl;
	flagy=true;}
	cout << "==========================" << endl;
	
	/****************************************
	 * Berechnen des Tiltwinkel Mittelwertes:
	 ****************************************/ 
	cout << "Tiltangle between tilted and untilted plane: " << endl;
	double TLTANG=0.0;
	for (i = 0; i < count;i++)
		{cout << " abs(angle" << i << "): " << tiltangle[i] <<endl;
		TLTANG+=tiltangle[i];
		}
	TLTANG/=count;
		cout << "------------------------------- \n abs( mean ): ";
		cout << TLTANG << " (4 mesurements)" << endl;
		//Berechnung des mittleren Fehlers des Mittelwertes:
		double angleSDV=0.0;
		for (i = 0; i < count;i++)
			{
			angleSDV+=(tiltangle[i]-TLTANG)*(tiltangle[i]-TLTANG);
			}
		angleSDV=sqrt(angleSDV/(count*(count-1)));
		cout << " mean error of mean: " << angleSDV << endl;
		cout << "===============================" << endl;
	/***********************************************************************************
	* Sign of tiltangle and interpretation of result:
	* 1. Case: Tiltaxis is parallel to y axis: Special conditions
	* 2. Case: Tiltaxis is not parallel to y-axis
	************************************************************************************/
		Vector tilt(0,0,0); //Vector in direction of tiltaxis
		Vector rM(k/2,k/2,M); //Center of horizontal plane
		// cout << " xmean: " << xmean << endl;
		// cout << " ymean: " << ymean << endl;
		tmpa.vSet(xmean,0.0,M);
		tmpb.vSet(0.0,ymean,M);
		tilt=tmpa-tmpb; //mean tiltaxis
		double dtmp=tilt.vScalar(rm4-rm1);
		if(dtmp < 0) {tilt=tmpb-tmpa;}
		cout << " Tiltaxis: " << tilt.x << ", " << tilt.y << ", " << tilt.z << endl;
		
	//Calculation of Angle between tiltaxis and x-axis seen from above on the negative:
		double TLTAXIS = rad2absgrad(tilt.vAngle(rm4-rm1));
		tmp=tilt.vCross(rm4-rm1);
		if (tmp.z > 0) {TLTAXIS*=-1;};
		if ( TLTAXIS > 90 ) cout << "Somthing wrong 1" << endl << endl;
		if ( TLTAXIS < -90 ) cout << "Somthing wrong 2" << endl << endl;
        cout << " TLTAXIS = X-axis -> Tilt-axis = " << TLTAXIS << endl;

//Calculation of Angle between lh and x-axis:
	double lhxAngle = rad2absgrad(lh.vAngle(rm4-rm1));
	tmp=lh.vCross(rm4-rm1);
	if (tmp.z < 0) {lhxAngle*=-1;};

//Calculation of Angle between lk and x-axis:
	double lkxAngle = rad2absgrad(lk.vAngle(rm4-rm1));
	tmp=lk.vCross(rm4-rm1);
	if (tmp.z < 0) {lkxAngle*=-1;};

//Calculation of handedness of lattice:
	double hand = lhxAngle - lkxAngle;
        if(hand<-180.0) {hand += 360.0;};
        if(hand> 180.0) {hand -= 360.0;};
        if(hand < 0)
          {hand = -1;
           if(irunmode != 4)
             rout << "  Handedness determined: left handed (" << hand << ")" << endl;}
        else
          {hand = 1;
           if(irunmode != 4)
           rout << "  Handedness determined: right handed (" << hand << ")" << endl;};
	
//TLTAXA: Calculation of Angle between tiltaxis anf lh on negative:
	double TLTAXA = rad2absgrad(tilt.vAngle(lh));
        tmp=tilt.vCross(lh);
	if (tmp.z < 0) {TLTAXA *= -1;};
        if (hand  < 0) {TLTAXA *= -1;};
	( TLTAXA > 90 ) ? TLTAXA = TLTAXA - 180 : TLTAXA=TLTAXA;
	( TLTAXA < -90 ) ? TLTAXA = TLTAXA + 180 : TLTAXA=TLTAXA;

//Calculation of sign of tiltangle: Is positive if p1 is weak underfocus and p2 is strong underfocus
	if ( flagy == false ) //General case
		{
		if ( (D1 + D4) - (D2 + D3) > 0 )
                	{ TLTANG*=-1;
	                  for (i = 0; i < count;i++) tiltangle[i]*=-1;
                        }
		};
	if ( flagy == true ) // The tiltaxis is parallel to the Y Axis
		{
		if ( (D1 + D2) - (D3 + D4) < 0 )
  			{ TLTANG*=-1;
                        for (i = 0; i < count;i++) tiltangle[i]*=-1;
                        }
		};

//TAXA: Calculation of Angle between tiltaxis and lh on sample:
        // rout << " TEST: TLTAXA            : " << TLTAXA << endl;
// Andreas Schenk:
	TAXA = atan(tan(TLTAXA*M_PI/180.0)*cos(TLTANG*M_PI/180.0))*180.0/M_PI;
        if(TAXA < 0) {TAXA *= -1;};
        if(TLTAXA < 0) {TAXA *= -1;};
        // rout << " TEST: TAXA nach AS      : " << TAXA << endl;

// Aus ttrefine.for:
        DENOM = sin(TLTAXA*M_PI/180.0)*sin(TLTANG*M_PI/180.0);
        DENOM = DENOM * DENOM;
        DENOM = sqrt(1.0 - DENOM);
        TAXA = acos(cos(TLTAXA*M_PI/180.0)/DENOM)*180.0/M_PI;
        if(TAXA < 0) {TAXA *= -1;};
        if(TLTAXA < 0) {TAXA *= -1;};
        // rout << " TEST: TAXA nach ttrefine: " << TAXA << endl;

/********************************
 * Final results and Commenting:
 ********************************/ 

       if(irunmode != '4')
         rout << endl; 

/* 
     TLTANG     magnitude of tiltangle.
                 (+ve for less underfocus at start of scan(y=0))
                 (or if TLTAXIS is parallel to y, at x=0)
*/

	rout << "- TLTANG: Tiltangle .................................. : " << TLTANG ;
        rout << " +/- " << angleSDV << endl;
        if(irunmode != 4)
	rout << "  (Old Tiltangle determined with .............. emtilt : " << oldTLTANG << " )" << endl;
	rout << "  (Values: " << tiltangle[0];
	for (i = 1; i < count;i++){rout << ", " << tiltangle[i];}; rout << ")" << endl;

	ofstream rou3( nam3, ios::out); //creates new outfile
	if (!nam3){
		cerr << "ERROR: Not able to create output file ";
		cerr << nam3 << ". File already exists \n";
		exit(-1);};

	//Comparison to old tiltangle und dem neu Berechneten:
	bool lflag=true; //new tilt angle differs a lot from old tilt angle
        if(irunmode != 4)
  	{ rout << "- Comparison new to old tiltangle: " << endl;
	  if (TLTANG * oldTLTANG < 0)
		{rout << endl;
                 rout << "  >>>>>>>>>>>>>> WARNING: ";
                 rout << "Old tiltangle has a different sign!";
		 rout << "  <<<<<<<<<<<<<<" << endl << endl;};
        };

        if (TLTANG < 0) { rou3 << "-1," << TLTAXIS << endl;}
        else { rou3 << "1," << TLTAXIS << endl;};

        tmp5=TLTANG-oldTLTANG;
        if(locTLTANG>0 && oldTLTANG<0){locTLTANG*=-1;};
        tmp7=locTLTANG-oldTLTANG;
        if(tmp7<0){tmp7*=-1;};
	if (tmp5 < 0){tmp5*=-1;};
	if (tmp5 <= 10.0 ) 
		{ lflag=false;
                  if(irunmode != 4)
                  rout << "  > Old tiltangle and new one are comparable." << endl;
                };
	if (lflag == true ) 
	    if (irunmode != 4 ) 
                {rout << endl;
                 rout << "  >>>>>>>>>>>>>> WARNING: ";
		 rout << "Old tiltangle and new one differ much.";
		 rout << "  <<<<<<<<<<<<<<" << endl << endl;}
	rout << "- Mean defocus: " << M << endl;
        if(irunmode != 4) rout << endl;
	rout << "- TLTAXIS: Angle x-Axis -> tiltaxis on negative ...... : " << TLTAXIS << endl;
        if(irunmode != 4)
	rout << "  (Old Angle determined with .................. emtilt : " << oldTLTAXIS << " )" << endl;

        // Comparison of the two versions of TLTAXIS from emtilt with the result. 
        // Which one is closer to the TLTAXIS result ?

        tmp1 = TLTAXI1 - TLTAXIS;
        if(tmp1 < 0.0) tmp1 = -tmp1;
        if(tmp1 > 90.0)
          {tmp1 -= 180.0;
           if(tmp1 < 0.0) tmp1 = -tmp1;};
        tmp2 = TLTAXI2 - TLTAXIS;
        if(tmp2 < 0.0) tmp2 = -tmp2;
        if(tmp2 > 90.0)
          {tmp2 -= 180.0;
           if(tmp2 < 0.0) tmp2 = -tmp2;};

        if(irunmode != 4)
        { rout << "  (First  emtilt result for TLTAXIS on negative ...... : " << TLTAXI1;
          rout << ", distance = " << tmp1 << " )" << endl;
          rout << "  (Second emtilt result for TLTAXIS on negative ...... : " << TLTAXI2;
          rout << ", distance = " << tmp2 << " )" << endl;
        };

	ofstream rou2( nam2, ios::out); //creates new outfile
	if (!nam2){
		cerr << "ERROR: Not able to create output file ";
		cerr << nam2 << ". File already exists \n";
		exit(-1);};

        if (tmp1 < tmp2 ) 
          {
           if(irunmode != 4)
           { rout << "  -----------------------------------------------------> ";
             rout << "First emtilt result is better." << endl;
             rou2 << "1" << endl;
           };
           tmp4=tmp1;}
        else
          { 
           if(irunmode != 4)
           { rout << "  -----------------------------------------------------> ";
             rout << "Second emtilt result is better." << endl;
             rou2 << "2" << endl;
           };
           tmp4=tmp2;};

	ofstream rou4( nam4, ios::out); //creates new outfile
	if (!nam4){
		cerr << "ERROR: Not able to create output file ";
		cerr << nam4 << ". File already exists \n";
		exit(-1);};
        rou4 << tmp4 << endl;
        // rout << " TILTAXIS distance = " << tmp4 << endl;

	ofstream rou5( nam5, ios::out); //creates new outfile
	if (!nam5){
		cerr << "ERROR: Not able to create output file ";
		cerr << nam5 << ". File already exists \n";
		exit(-1);};
        rou5 << tmp5 << endl;
        // rout << " Tiltangle distance = " << tmp5 << endl;

	ofstream rou6( nam6, ios::out); //creates new outfile
	if (!nam6){
		cerr << "ERROR: Not able to create output file ";
		cerr << nam6 << ". File already exists \n";
		exit(-1);};
        rou6 << TLTAXIS << endl;
        rou6 << TLTANG  << endl;
        if ((-361<TLTAXA) && (TLTAXA<361.0)){
          rou6 << TLTAXA  << endl;
          rou6 << TAXA    << endl;
        } else {
          rou6 << 0.0     << endl;
          rou6 << 0.0     << endl;
        };

        ofstream rou7( nam7, ios::out); //creates new outfile
        if (!nam7){
                cerr << "ERROR: Not able to create output file ";
                cerr << nam7 << ". File already exists \n";
                exit(-1);};
        rou7 << tmp7 << endl;
        // rout << " Local Tiltangle distance = " << tmp5 << endl;


// Calculate TANGL with correct sign:

        ANGA = -lhxAngle;
        ANGB = -lkxAngle;
        ANGDIF = ANGB-ANGA;
        ABSANGDIF = ANGDIF;
        if(ABSANGDIF<0){ABSANGDIF*=-1;};
        if(ABSANGDIF>180.0)
        { if(ANGDIF>0)
          {ANGB-=360.0;}
          else
          {ANGB+=360.0;};
        }
        if(ANGB-ANGA>0.0)
        { HAND=1.0; }
        else
        { HAND=-1.0;};
        
        // rout << " ANGA = " << ANGA << endl;
        // rout << " ANGB = " << ANGB << endl;
        // rout << " ANGDIF = " << ANGDIF << endl;
        // rout << " HAND = " << HAND << endl;
          
        TLTNORM = TLTAXIS + 90.0;
        ANGACOMP = ANGA - TLTNORM;

        if(ANGACOMP<0) {ANGACOMP *= -1;};

        if((ANGACOMP>90.0)&&(ANGACOMP<270.0))
        { AISABOVE = -1; }
        else
        { AISABOVE = 1; };
        // rout << " AISABOVE = " << AISABOVE << endl;

        if(TLTAXA<0)
        { SIGNTLTAXA = -1;}
        else
        { SIGNTLTAXA = 1;};
        // rout << " SIGNTLTAXA = " << SIGNTLTAXA << endl;

        TANGL = (AISABOVE*SIGNTLTAXA*HAND) * TLTANG;
        // rout << " TLTANG = " << TLTANG << endl;
        // rout << " TANGL  = " << TANGL  << endl;

        rou6 << TANGL         << endl;



 
        if(irunmode != 4)
        { rout << endl;
	  rout << "- TLTAXA: Angle tiltaxis -> A axis on negative ....... : " << TLTAXA << " )" << endl;
          rout << "  (Old Angle determined with .................. emtilt : " << oldTLTAXA << " )" << endl;
          rout << endl;
	  rout << "  (Angle between h and x-axis: " << lhxAngle << ")" << endl;
        };
	cout << "New tiltangle: " << TLTANG << endl;
	cout << "------------------------------------------------------------" << endl ;
	cout << "deftilt end" << endl;
	cout << "============================================================" << endl ;
	cout << "============================================================" << endl ;
	return 0;
}

