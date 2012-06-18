/***************************************************************************************
****************************************************************************************
  2dx_tiltgeom2.cc

  Uses vGeometry.h (vs 2.0).
  Use "CC -ansi deftilt.C -lm" to compile. vGeometry.h has to be in the same directory.
****************************************************************************************
****************************************************************************************/
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <iomanip>
#include "vGeometry.h"
using namespace std;
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
//converts rad to grad, sign is kept
{return (a/3.1415926535897932)*180.0;};

inline double grad2rad(double a)
//converts grad to rad, sign is kept
{return (a*3.1415926535897932)/180.0;};

/******************************************************************************************
Start of main program
*******************************************************************************************/ 
main()
{
	cout << endl;
	cout << ":Program to calculate remaining tiltgeometry values" << endl;
 	cout << ":--------------------------------------------------" << endl;
	/************************
 	* Input of Parameters: 
	************************/
	double TLTANG,TLTAXA;
        double TLTAXIS,TAXA;
        double DENOM,ANGA,ANGB,ANGDIF,ABSANGDIF,HAND,TLTNORM,ANGACOMP,AISABOVE;
        double SIGNTLTAXA,TANGL;
	double u1,u2,v1,v2;
	char vectors[80];

	cout << endl;

	cout << "Coordinates of u,v vectors: ";
	cin >> vectors;
        cout << vectors << endl;

	int z=sscanf(vectors,"%lf,%lf,%lf,%lf",&u1,&u2,&v1,&v2);
	if (z != 4 ){
		cerr <<"Wrong input for u, v vectors. \n Example: 12.3,5,3.1,6 \n";
		exit(-1);};

	cout << "TLTAXIS: ";
	cin >> TLTAXIS;
        cout << TLTAXIS << endl;

	cout << "TLTANG: ";
	cin >> TLTANG;
        cout << TLTANG << endl;

	cout << endl;

 /*****************************
 * Construction of Vectors:
 ******************************/
 	Vector tmp(0,0,0); //Vector for various purposes
 	Vector tmpa(0,0,0); //Vector for various purposes
 	Vector tmpb(0,0,0); //Vector for various purposes

	//lattice vectors: pointvector - rm1
	Vector lh(u1,u2,0.0);
	Vector lk(v1,v2,0.0);

        //vectors to horizontal plane:
        Vector rm1(0.0,0.0,1.0); //Direction: Tiltscanaxis
        Vector rm2(0.0,1.0,1.0);
        Vector rm3(1.0,1.0,1.0);
        Vector rm4(1.0,0.0,1.0);
	
	/***********************************************************************************
	* Sign of tiltangle and interpretation of result:
	* 1. Case: Tiltaxis is parallel to y axis: Special conditions
	* 2. Case: Tiltaxis is not parallel to y-axis
	************************************************************************************/
    
	// cout << "TLTAXIS = " << TLTAXIS << endl;
	// cout << "grad2rad(TLTAXIS) = " << grad2rad(TLTAXIS) << endl;
	// cout << "sin(TLTAXIS) = " << sin(grad2rad(TLTAXIS)) << endl;

	Vector tilt(cos(grad2rad(TLTAXIS)),sin(grad2rad(TLTAXIS)),0); //Vector in direction of tiltaxis
	cout << " Tiltaxis: " << tilt.x << ", " << tilt.y << ", " << tilt.z << endl;
		
	//Calculation of Angle between tiltaxis and x-axis seen from above on the negative:
		TLTAXIS = rad2absgrad(tilt.vAngle(rm4-rm1));
		tmp=tilt.vCross(rm4-rm1);
		if (tmp.z > 0) {TLTAXIS*=-1;};
		if ( TLTAXIS > 90 ) cout << ":: Somthing wrong 1" << endl << endl;
		if ( TLTAXIS < -90 ) cout << ":: Somthing wrong 2" << endl << endl;
        cout << ": TLTAXIS after recalculation = X-axis -> Tilt-axis = " << TLTAXIS << endl;

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
           cout << ":Handedness determined: left handed (" << hand << ")" << endl;}
        else
          {hand = 1;
           cout << ":Handedness determined: right handed (" << hand << ")" << endl;};
	
//TLTAXA: Calculation of Angle between tiltaxis and lh on negative:
	TLTAXA = rad2absgrad(tilt.vAngle(lh));
        tmp=tilt.vCross(lh);
	if (tmp.z < 0) {TLTAXA *= -1;};
        if (hand  < 0) {TLTAXA *= -1;};
	( TLTAXA > 90 ) ? TLTAXA = TLTAXA - 180 : TLTAXA=TLTAXA;
	( TLTAXA < -90 ) ? TLTAXA = TLTAXA + 180 : TLTAXA=TLTAXA;

//TAXA: Calculation of Angle between tiltaxis and lh on sample:
        cout << " TLTAXA            : " << TLTAXA << endl;
	TAXA = atan(tan(TLTAXA*M_PI/180.0)*cos(TLTANG*M_PI/180.0))*180.0/M_PI;
        if(TAXA < 0) {TAXA *= -1;};
        if(TLTAXA < 0) {TAXA *= -1;};
        cout << " TAXA nach AS      : " << TAXA << endl;

// From ttrefine.for:
        DENOM = sin(TLTAXA*M_PI/180.0)*sin(TLTANG*M_PI/180.0);
        DENOM = DENOM * DENOM;
        DENOM = sqrt(1.0 - DENOM);
        TAXA = acos(cos(TLTAXA*M_PI/180.0)/DENOM)*180.0/M_PI;
        if(TAXA < 0) {TAXA *= -1;};
        if(TLTAXA < 0) {TAXA *= -1;};
        cout << " TAXA after ttrefine: " << TAXA << endl;


/* 
     TLTANG     magnitude of tiltangle.
                 (+ve for less underfocus at start of scan(y=0))
                 (or if TLTAXIS is parallel to y, at x=0)
*/

	cout << " TLTANG: " << TLTANG << endl;
        cout << " TLTAXIS: Angle x-Axis -> tiltaxis on negative: " << TLTAXIS << endl;


// Calculate TANGL with correct sign:

        ANGA = -lhxAngle;
        ANGB = -lkxAngle;
        ANGDIF = ANGB-ANGA;
        ABSANGDIF = ANGDIF;
        if(ABSANGDIF<0){ABSANGDIF*=-1;};
        if(ABSANGDIF>180.0) { if(ANGDIF>0) {ANGB-=360.0;} else {ANGB+=360.0;}; }
        if(ANGB-ANGA>0.0) { HAND=1.0; } else { HAND=-1.0;};

        cout << " ANGA = " << ANGA << endl;
        cout << " ANGB = " << ANGB << endl;
        cout << " ANGDIF = " << ANGDIF << endl;
        cout << " HAND = " << HAND << endl;
          
        TLTNORM = TLTAXIS + 90.0;
        ANGACOMP = ANGA - TLTNORM;

        if(ANGACOMP<0) {ANGACOMP *= -1;};

        if((ANGACOMP>90.0)&&(ANGACOMP<270.0)) { AISABOVE = -1; } else { AISABOVE = 1; };
        cout << ": AISABOVE = " << AISABOVE << endl;

        if(TLTAXA<0) { SIGNTLTAXA = -1;} else { SIGNTLTAXA = 1;};
        cout << ": SIGNTLTAXA = " << SIGNTLTAXA << endl;

        TANGL = (AISABOVE*SIGNTLTAXA*HAND) * TLTANG;
        cout << ": TLTANG = " << TLTANG << endl;
        cout << ": TANGL  = " << TANGL  << endl;
 
	cout << ": TLTAXA: Angle tiltaxis -> A axis on negative ....... : " << TLTAXA << " )" << endl;
	cout << ": TLTANG: " << TLTANG << endl;

        cout << ":----------------------------------------------------------------" << endl;
        cout << endl;
        cout << ":Final results:" << endl;
        cout << endl;
        cout << ":TLTAXIS = " << TLTAXIS << endl;
        cout << ":TLTANG  = " << TLTANG  << endl;
        cout << "::TLTAXA  = " << TLTAXA  << endl;
        cout << "::TAXA    = " << TAXA    << endl;
        cout << "::TANGL   = " << setprecision(4) << TANGL   << endl;

        ofstream resu( "2dx_tiltgeom2.out", ios::out); //creates new outfile
        if (!resu)
        {
                cerr << "ERROR: Not able to create output file 2dx_tiltgeom.out ."<< endl;
                cerr  << "File already exists." << endl;;
                exit(-1);
        };
        resu << "set TLTAXIS = " << TLTAXIS << endl;
        resu << "set TLTANG  = " << TLTANG  << endl;
        resu << "set TLTAXA  = " << TLTAXA  << endl;
        resu << "set TAXA    = " << TAXA    << endl;
        resu << "set TANGL   = " << setprecision(4) << TANGL   << endl;



	return 0;
}

