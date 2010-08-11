#ifndef _MYGEOMETRY_
#define _MYGEOMETRY_ 1
#include "myvector.h"


template <class T>
class Cline
{
private:
public:
	Cvector<T> r; 												// starting point
	Cvector<T> s; 												// direction vector
	Cline(Cvector<T> R,Cvector<T> S):r(3),s(3) { r = R; s = S; }
	bool parallel(Cvector<T> &v);			
	bool parallel(Cline<T> &l);			
};


template <class T>
class Cplane
{
private:
public:
	Cvector<T> a; 												// starting point
	Cvector<T> n; 												// normal vector
	Cplane(Cvector<T> A,Cvector<T> N):a(3),n(3)
	{
		n = N;
		a = A;
	}

	Cplane(Cvector<T> v1,Cvector<T> v2,Cvector<T> v3)
	{
		Cvector<T> x(3);
		x=v3-v1;
		Cvector<T> y(3);
		y=v2-v1;
		n=cross(x,y);
		if(n[2]<0)
			n=n*(-1);
		a=v1;
	}
	bool parallel(Cplane<T> &B);
	bool parallel(Cvector<T> &v);								//checks, if a vector is parallel to the plane
	bool parallel(Cline<T> &l);			
	double angle(Cplane<T> &B); 								// angle between two planes
	double sangle(Cplane<T> &B); 								// angle between two planes
	Cvector<T> crosspoint(Cline<T> &l); 						// Crosspoint Plane-Line
	Cvector<T> crosspoint(Cvector<T> &r,Cvector<T> &s); 		// Crosspoint Plane-Line
};
//______________________________________________________________________
template <class T>
bool Cplane<T>::parallel(Cplane<T> &B) 							// planes parallel or identical
{
	Cvector<T> v = cross(n,B.n);
	return (v.length()== 0.) ? true : false;
}
//______________________________________________________________________
template <class T>
double Cplane<T>::angle(Cplane<T> &B)
{
	return ::angle(n,B.n);
}
//______________________________________________________________________
template <class T>
double Cplane<T>::sangle(Cplane<T> &B)
{
	return ::sangle(n,B.n);
}
//______________________________________________________________________
template <class T>
Cvector<T> Cplane<T>::crosspoint(Cline<T> &l)
{
double scal=dot(n,l.s);
	if (scal == 0 )
	{
		cerr <<  "ERROR: Plane and line are parallel!" << endl;
	}
	double t=dot(n,a-l.r)/scal; 								/*How many times the direction vector of the line has to be stretched to meet the plane*/
	return l.r+t*l.s;
}

template <class T>
Cvector<T> Cplane<T>::crosspoint(Cvector<T> &r,Cvector<T> &s)
{
double scal=dot(n,s);
	if (scal == 0 )
	{
		cerr <<  "ERROR: Plane and line are parallel!" << endl;
	}
	double t=dot(n,a-r)/scal; 									/*How many times the direction vector of the line has to be stretched to meet the plane*/
	return r+t*s;
}
//______________________________________________________________________
template <class T>
bool Cplane<T>::parallel(Cvector<T> &a) 						// planes parallel or identical
{
	return (dot(n,a) == 0.) ? true : false;
}

template <class T>
double angle(Cplane<T> &A,Cplane<T> &B)							// angle between two planes
{
	return A.angle(B);
}
template <class T>
double sangle(Cplane<T> &A,Cplane<T> &B)							// angle between two planes
{
	return A.sangle(B);
}


double mod(double a,double b)
{
	return a-((int)(a/b))*b;
}
double reduce_angle(double angle,double base)
{
	return mod(mod(angle+base/2,base)-base,base)+base/2;
}
#endif
