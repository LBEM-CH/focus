#define maxdim 101

class Chkamparray
{
	double amp[maxdim][maxdim];
	char title[100];
public:
	Chkamparray(void);
	Chkamparray(char *file);
	Chkamparray& operator=(Chkamparray tmp);
	Chkamparray operator+(Chkamparray tmp);
	Chkamparray operator*(double n);
	void revhk();
	void rmneg();
	void rmones();
	void gettitle(char *);
	void puttitle(char *);
	double getamp(int h,int k);	
	char exists(int h,int k);	
	void putamp(int h,int k, double a);	
	void readlst(char *file);	
	void readp4lst(char *file);	
	void writelst(char *file);
	void makepx(char num);	
};
class Chkampsigarray:public Chkamparray
{
	double sig[maxdim][maxdim];
public:
	Chkampsigarray(void);
	Chkampsigarray(char *file);
	Chkampsigarray& operator=(Chkampsigarray tmp);
	Chkampsigarray& operator=(Chkamparray tmp);
	Chkampsigarray operator+(Chkampsigarray tmp);
	void revhk();
	double getsig(int h,int k);	
	void putsig(int h,int k, double s);	
	void readlst(char *file);	
	void readp4lst(char *file);	
	void writelst(char *file);	
	void makepx(char num);	
};

class Chkampphafomarray:public Chkamparray
{
	double pha[maxdim][maxdim];
	double fom[maxdim][maxdim];
public:
	Chkampphafomarray(void);
	Chkampphafomarray(char *file);
	double getpha(int h,int k);	
	void putpha(int h,int k, double p);	
	double getfom(int h,int k);	
	void putfom(int h,int k, double f);	
	void readlst(char *file);	
	void readp4lst(char *file);	
	void writelst(char *file);	
	void makepx(char num);	
};

double getscalefactor(Chkamparray a1,Chkamparray a2);
double getrmerge(Chkamparray a1,Chkamparray a2);
