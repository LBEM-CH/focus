/*************************************************************************
vGeometry.h V2: Class definitions for calculations with Vectors,lines and
planes.
WARNING: Not all implementations were tested
13.11.99	Implementation of Class Vector
22.11.99	Implementation of Class plane and corresponding Operations
**************************************************************************/  
#include <iostream>
#include <math.h>
using namespace std;
/*************************************************************************
Vectors in 3D space
**************************************************************************/
class Vector
{
private:
	
public:
	double x,y,z;
	Vector() {}
	Vector(double X,double Y,double Z){ x=X; y=Y; z=Z;};
	void vSet(double x,double y,double z); //Changing x,y,z, values directly
	Vector operator+(Vector);
	Vector operator-(Vector);
    	Vector vStretch(double); // skal.Multiplikation
	double vScalar(Vector); // Skalarprodukt
	Vector vCross(Vector);
	double vAngle(Vector);  
	double vLength() {return sqrt(x*x+y*y+z*z);}
friend class Plane;
};
ostream& operator <<(ostream& os, Vector& v);
//______________________________________________________________________
ostream& operator <<(ostream& os, Vector& v)
{
	os << "(" << v.x  << ","<< v.y  << ","<< v.z  << ")";
	return os;
}
//______________________________________________________________________
void Vector::vSet(double X,double Y,double Z){x=X;y=Y;z=Z;}
//______________________________________________________________________
Vector Vector::operator+(Vector a)
{
Vector r;
r.x = x+a.x;
r.y = y+a.y;
r.z = z+a.z;
return r;
}
//______________________________________________________________________
Vector Vector::operator-(Vector a)
{
Vector r;
r.x = x-a.x;
r.y = y-a.y;
r.z = z-a.z;
return r;
}
//______________________________________________________________________
Vector Vector::vStretch(double f)
{
	Vector r;
	r.x = x*f;
	r.y = y*f;
	r.z = z*f;
return r;
}
//______________________________________________________________________                
double Vector::vScalar(Vector a)
{ 
return x*a.x+y*a.y+z*a.z; 
}                       
//______________________________________________________________________  
Vector Vector::vCross(Vector a)
{
Vector r;
r.x = y*a.z-z*a.y;
r.y = z*a.x-x*a.z;
r.z = x*a.y-y*a.x;
return r;
}
//______________________________________________________________________  
double Vector::vAngle(Vector a)
{
	double scalarpeodukt=x*a.x+y*a.y+z*a.z;
	double lengthproduct=sqrt(x*x+y*y+z*z)*sqrt(a.x*a.x+a.y*a.y+a.z*a.z);
	return acos(scalarpeodukt/lengthproduct);
}
	
/*************************************************************************
Class Plane: Plane in the 3d space. Vector a is starting point of plane, 
n is normal vector
**************************************************************************/
class Plane
{
private:
public:
Vector a; // starting point
Vector n; // normal vector
Plane(Vector N,Vector A) { n = N; a = A; }
Plane(Vector v1,Vector v2,Vector v3)
	{Vector x=v3-v1;
	Vector y=v2-v1;
	n=x.vCross(y);
	a=v1;
	}
bool pParallel(Plane &B);
double pAngle(Plane &B); // angle between two planes
Vector pCrosspoint(Vector, Vector); // Crosspoint Plane-Line
bool pvParallel(Vector); //checks, if a vector is parallel to the plane
};
//______________________________________________________________________
bool Plane::pParallel(Plane &B) // planes parallel or identical
{
Vector v = n.vCross(B.n);
return (v.vLength()== 0.) ? true : false;
}
//______________________________________________________________________
double Plane::pAngle(Plane &B)
{
return n.vAngle(B.n);
}
//______________________________________________________________________
Vector Plane::pCrosspoint(Vector r, Vector d)
/*r: starting point of Line; d: direction of line*/
{
double devider=n.vScalar(d);
if (devider == 0 )
{
cerr <<  "ERROR: pCrosspoint (lib: vGeometry.h): \n    Plane and line are parallel!" << endl;
exit(-1);
}
double s=n.vScalar(a-r)/devider; /*How many times the direction vector of 
the line has to be stretched to meet the plane*/
return r+d.vStretch(s);
}
//______________________________________________________________________
bool Plane::pvParallel(Vector a) // planes parallel or identical
{
return (n.vScalar(a) == 0.) ? true : false;
}
