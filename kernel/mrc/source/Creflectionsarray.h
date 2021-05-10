#define initdimensions 20
#include <stdlib.h>
#include <string>
#include <fstream>
#include <algorithm>
#include <vector>


using namespace std;
class CException
{
private:
	string message;
public:
	CException( string m );
	void Report();
};

class Creflectionsarray
{
private:
	double *buf;
	int dim;
public:
	Creflectionsarray(void);
	Creflectionsarray( Creflectionsarray &tmp);
	~Creflectionsarray(void);
	Creflectionsarray& operator=(Creflectionsarray &tmp);
	unsigned getdim();
	double get(int h,int k, int l);
	void put(int h,int k, int l,double val);
	void grow(unsigned size);
	void shrink(unsigned size);
	void setsize(unsigned size);
	double & operator() (int h, int k, int l);
	void rotate(char axis,int num);
};
class Cphasearray:public Creflectionsarray
{
private:
	double reduce(double val);
	double rest(double a,double b);// a%b for double
public:
	void put(int h,int k, int l,double val);
	double get(int h,int k, int l);
	double & operator() (int h, int k, int l);
	void rotate(char axis,int num);
};
class Creflectionset
{
private:
	string title;
public:
	Creflectionsarray amp,fom;
	Cphasearray pha; 
	Creflectionset(void);
	void put(vector<string> tokens,vector<double> data);
	void getasstring(char *buf,int h,int k,int l,string token);
	void read(istream &infile,string format);
	void readhkl(istream &infile);
	void write(ostream &outfile,string format);
	void writehkl(ostream &outfile);
	void readhk(istream &infile);
	void readaph(istream &infile);
	void writehk(ostream &outfile);
	void writeaph(ostream &outfile);
	string gettitle(void);
	void puttitle(string t);
	void permutate(int cw);
	void rotate(char axis,int num);
};
void Tokenize(const string& str,vector<string>& tokens,const string& delimiters);
