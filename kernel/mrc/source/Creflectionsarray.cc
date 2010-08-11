#include <iostream>
#include <fstream>
#include <cstring>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Creflectionsarray.h"
inline int max(int x,int y) {return x > y ? x : y;}
inline int min(int x,int y) {return x < y ? x : y;}
inline int sign(int x) {return x >= 0 ? 1 : -1;}

//-----------------------------------CException-------------------------------------------------
CException::CException( string m )
{ 
	message = m;
}
void CException::Report()
{
	cerr <<endl;
	cerr << message <<endl;
	exit(1);
}

//-----------------------------------CReflectionsarray-------------------------------------------------
Creflectionsarray::Creflectionsarray(void)
{
	try
	{
		dim=initdimensions;
		buf=(double *) malloc(sizeof(double)*(dim+1)*(2*dim+1)*(2*dim+1));
		if(buf==NULL)
				throw CException("malloc failed: set inidimensions to a lower value");
		memset(buf,0,sizeof(double)*(dim+1)*(2*dim+1)*(2*dim+1));
 	}
	catch(CException e)
	{
		e.Report();
	}
}
Creflectionsarray::~Creflectionsarray(void)
{
	free(buf);
}
Creflectionsarray::Creflectionsarray(Creflectionsarray &tmp)
{
	try
	{
		dim=tmp.getdim();
		buf=(double *) malloc(sizeof(double)*(dim+1)*(2*dim+1)*(2*dim+1));
		if(buf==NULL)
				throw CException("malloc failed: set inidimensions to a lower value");
		memcpy(buf,tmp.buf,sizeof(double)*(dim+1)*(2*dim+1)*(2*dim+1));
	}
	catch(CException e)
	{
		e.Report();
	}
}
Creflectionsarray& Creflectionsarray::operator =(Creflectionsarray &tmp)
{
	try
	{
		if(this != &tmp)
			dim=tmp.getdim();
			free(buf);
			buf=(double *) malloc(sizeof(double)*(dim+1)*(2*dim+1)*(2*dim+1));
			if(buf==NULL)
					throw CException("malloc failed: set inidimensions to a lower value");
			memcpy(buf,tmp.buf,sizeof(double)*(dim+1)*(2*dim+1)*(2*dim+1));
		return *this;
	}
	catch(CException e)
	{
		e.Report();
	}
}
unsigned Creflectionsarray::getdim(void)
{
	return dim;
}
double Creflectionsarray::get(int h,int k, int l)
{
	unsigned maxhkl,maxkl;
	if(h<0 || h==0 && k<0)
		h*=-1,k*=-1,l*=-1;
	maxkl=max(abs(k),abs(l));
	maxhkl=max(maxkl,abs(h));
	if(maxhkl>dim)
	{
		char s[15];
		sprintf(s,"%4d%4d%4d",h,k,l);
		throw CException("Bad Index:"+(string)s);
	}
	return *(buf+maxhkl*(2*maxhkl-1)*(2*maxhkl-1)+8*h*maxhkl+4*maxkl+sign(maxkl-1+l)*(maxkl-k)+min(-sign(k-maxkl),sign(l+maxkl-1))*(maxkl-l)+(h== (int) maxhkl)*(k!=0 || l!=0)*(2*maxkl-1)*(2*maxkl-1));
}

void Creflectionsarray::put(int h,int k, int l,double val)
{
	unsigned maxhkl,maxkl;
	if(h<0)
		h*=-1,k*=-1,l*=-1;
	maxkl=max(abs(k),abs(l));
	maxhkl=max(maxkl,abs(h));
	if(maxhkl>dim)
		setsize(maxhkl);
	*(buf+maxhkl*(2*maxhkl-1)*(2*maxhkl-1)+8*h*maxhkl+4*maxkl+sign(maxkl-1+l)*(maxkl-k)+min(-sign(k-maxkl),sign(l+maxkl-1))*(maxkl-l)+(h==(int) maxhkl)*(k!=0 || l!=0)*(2*maxkl-1)*(2*maxkl-1))=val;
}
void Creflectionsarray::grow(unsigned size)
{
	try
	{
		double *tp;
		tp=(double *) realloc(buf,sizeof(double)*(dim+1+size)*(2*(dim+size)+1)*(2*(dim+size)+1));
		if(tp==NULL)
		{
				char s[10];
				sprintf(s,"%d",size);
				throw CException((string)"set size:realloc failed. Size was "+(string)s);
		}
		buf=tp;
		memset(buf+(dim+1)*(2*dim+1)*(2*dim+1),0,sizeof(double)*((dim+1+size)*(2*(dim+size)+1)*(2*(dim+size)+1)-(dim+1)*(2*dim+1)*(2*dim+1)));
		dim=dim+size;
	}
	catch(CException e)
	{
		e.Report();
	}
	
}
void Creflectionsarray::shrink(unsigned size)
{
	buf=(double *) realloc(buf,sizeof(double)*(dim+1-size)*(2*(dim-size)+1)*(2*(dim-size)+1));
	dim=dim-size;
	
}
void Creflectionsarray::setsize(unsigned size)
{
	try
	{
		double *tp;
		tp=(double *) realloc(buf,sizeof(double)*(size+1)*(2*(size)+1)*(2*(size)+1));
		if(tp==NULL)
		{
			char s[10];
			sprintf(s,"%d",size);
			throw CException((string)"set size:realloc failed. Size was "+(string)s);
		}
		buf=tp;
		if(dim<size)
			memset(buf+(dim+1)*(2*dim+1)*(2*dim+1),0,sizeof(double)*((size+1)*(2*size+1)*(2*size+1)-(dim+1)*(2*dim+1)*(2*dim+1)));
		dim=size;
	}
	catch(CException e)
	{
		e.Report();
	}
}

double& Creflectionsarray::operator() (int h, int k, int l)
{
	unsigned maxhkl,maxkl;
	if(h<0)
		h*=-1,k*=-1,l*=-1;
	maxkl=max(abs(k),abs(l));
	maxhkl=max(maxkl,abs(h));
	if(maxhkl>dim)
	{
		setsize(maxhkl);
	}
	return *(buf+maxhkl*(2*maxhkl-1)*(2*maxhkl-1)+8*h*maxhkl+4*maxkl+sign(maxkl-1+l)*(maxkl-k)+min(-sign(k-maxkl),sign(l+maxkl-1))*(maxkl-l)+(h==(int) maxhkl)*(k!=0 || l!=0)*(2*maxkl-1)*(2*maxkl-1));	 
}

void Creflectionsarray::rotate(char axis,int num)
{
	try
	{
		Creflectionsarray tmp;
		int k,h,l,i;
		switch(axis)
		{
		case 'h':
		case 'H':
			if(num>0)
			{
				for(i=0;i<num;i++)
				{
					tmp=*this;
					for(h=0;h<=dim;h++)
						for(k=-dim;k<=dim;k++)
							for(l=-dim;l<=dim;l++)
							{
								put(h,k,l,tmp.get(h,-l,k));
							}
				}
			}
			else
				if(num<0)
				{
					for(i=0;i>num;i--)
					{
						tmp=*this;
						for(h=0;h<=dim;h++)
							for(k=-dim;k<=dim;k++)
								for(l=-dim;l<=dim;l++)
								{
									put(h,k,l,tmp.get(h,l,-k));
								}
					}
				}
			break;
		case 'k':
		case 'K':
			if(num>0)
			{
				for(i=0;i<num;i++)
				{
					tmp=*this;
					for(h=0;h<=dim;h++)
						for(k=-dim;k<=dim;k++)
							for(l=-dim;l<=dim;l++)
							{
								put(h,k,l,tmp.get(-l,k,h));
							}
				}
			}
			else
				if(num<0)
				{
					for(i=0;i>num;i--)
					{
						tmp=*this;
						for(h=0;h<=dim;h++)
							for(k=-dim;k<=dim;k++)
								for(l=-dim;l<=dim;l++)
								{
									put(h,k,l,tmp.get(l,k,-h));
								}
					}
				}
			break;
		case 'l':
		case 'L':
			if(num>0)
			{
				for(i=0;i<num;i++)
				{
					tmp=*this;
					for(h=0;h<=dim;h++)
						for(k=-dim;k<=dim;k++)
							for(l=-dim;l<=dim;l++)
							{
								put(h,k,l,tmp.get(-k,h,l));
							}
				}
			}
			else
				if(num<0)
				{
					for(i=0;i>num;i--)
					{
						tmp=*this;
						for(h=0;h<=dim;h++)
							for(k=-dim;k<=dim;k++)
								for(l=-dim;l<=dim;l++)
								{
									put(h,k,l,tmp.get(k,-h,l));
								}
					}
				}
			break;
		}
	}
	catch(CException e)
	{
		e.Report();
	}
}
//-----------------------------------Cphasearray-------------------------------------------------
void Cphasearray::put(int h,int k, int l,double val)
{
	if(h<0)
	{
		h*=-1,k*=-1,l*=-1;
		val*=-1;
	}
	Creflectionsarray::put(h,k,l,reduce(val));	
}
double Cphasearray::get(int h,int k, int l)
{
	double val;
	val=reduce(Creflectionsarray::get(h,k,l));	
	if(h<0 || h==0 && k<0)
		val*=-1;
	return val;
}
double Cphasearray::reduce(double val)
{
	return rest(val+180+5*360,360)-180;
}
double Cphasearray::rest(double a,double b)
{
	return (a/b-((double)(int)(a/b)))*b;
}
double& Cphasearray::operator() (int h, int k, int l)
{
	try
	{
		unsigned maxhkl,maxkl;
		maxkl=max(abs(k),abs(l));
		maxhkl=max(maxkl,abs(h));
		if(maxhkl>getdim())
		{
			setsize(maxhkl);
		}
		put(h,k,l,get(h,k,l));
		if(h<0)
			throw CException("negative h\n");
		double &ref=Creflectionsarray::operator() (h,k,l);
		return ref;
	}
	catch(CException e)
	{
		e.Report();
	}
}

void Cphasearray::rotate(char axis,int num)
{
	try
	{
		Cphasearray tmp;
		int k,h,l,i,dim;
		dim=getdim();
		switch(axis)
		{
		case 'h':
		case 'H':
			if(num>0)
			{
				for(i=0;i<num;i++)
				{
					tmp=*this;
					for(h=0;h<=dim;h++)
						for(k=-dim;k<=dim;k++)
							for(l=-dim;l<=dim;l++)
							{
								put(h,k,l,tmp.get(h,-l,k));
							}
				}
			}
			else
				if(num<0)
				{
					for(i=0;i>num;i--)
					{
						tmp=*this;
						for(h=0;h<=dim;h++)
							for(k=-dim;k<=dim;k++)
								for(l=-dim;l<=dim;l++)
								{
									put(h,k,l,tmp.get(h,l,-k));
								}
					}
				}
			break;
		case 'k':
		case 'K':
			if(num>0)
			{
				for(i=0;i<num;i++)
				{
					tmp=*this;
					for(h=0;h<=dim;h++)
						for(k=-dim;k<=dim;k++)
							for(l=-dim;l<=dim;l++)
							{
								put(h,k,l,tmp.get(-l,k,h));
							}
				}
			}
			else
				if(num<0)
				{
					for(i=0;i>num;i--)
					{
						tmp=*this;
						for(h=0;h<=dim;h++)
							for(k=-dim;k<=dim;k++)
								for(l=-dim;l<=dim;l++)
								{
									put(h,k,l,tmp.get(l,k,-h));
								}
					}
				}
			break;
		case 'l':
		case 'L':
			if(num>0)
			{
				for(i=0;i<num;i++)
				{
					tmp=*this;
					for(h=0;h<=dim;h++)
						for(k=-dim;k<=dim;k++)
							for(l=-dim;l<=dim;l++)
							{
								put(h,k,l,tmp.get(-k,h,l));
							}
				}
			}
			else
				if(num<0)
				{
					for(i=0;i>num;i--)
					{
						tmp=*this;
						for(h=0;h<=dim;h++)
							for(k=-dim;k<=dim;k++)
								for(l=-dim;l<=dim;l++)
								{
									put(h,k,l,tmp.get(k,-h,l));
								}
					}
				}
			break;
		}
	}
	catch(CException e)
	{
		e.Report();
	}
}

//-----------------------------------Creflectionset-------------------------------------------------
Creflectionset::Creflectionset()
{
	title="";
}
void Creflectionset::put(vector<string> tokens,vector<double> data)
{
	int i=0,h=0,k=0,l=0;
	double a=0,p=0,f=0;
	for(i=0;i<tokens.size();i++)
	{
		switch(tokens[i][0])
		{
		case 'h':
			h=(int)data[i];
			break;
		case 'k':
			k=(int)data[i];
			break;
		case 'l':
			l=(int)data[i];
			break;
		case 'a':
			a=data[i];
			break;
		case 'p':
			p=data[i];
			break;
		case 'f':
			f=data[i];
			break;
		default:
			break;
		}
	}
	amp.put(h,k,l,a);
	pha.put(h,k,l,p);
	fom.put(h,k,l,f);
}
void Creflectionset::getasstring(char *tmp,int h,int k,int l,string token)
{
//	for(int i=0;i<token.size();i++)
//	{
		switch(token[0])
		{
		case 'h':
			sprintf(tmp,"%4d",h);
			break;
		case 'k':
			sprintf(tmp,"%4d",k);
			break;
		case 'l':
			sprintf(tmp,"%4d",l);
			break;
		case 'a':
			sprintf(tmp,"%8.1f",amp.get(h,k,l));
			break;
		case 'p':
			sprintf(tmp,"%7.1f",pha.get(h,k,l));
			break;
		case 'f':
			sprintf(tmp,"%8.3f",fom.get(h,k,l));
			break;
		default:
			break;
		}
//	}
}
void Creflectionset::read(istream &infile,string format)
{
	try
	{
    		vector<string> tokens; 
		vector<double> data; 
		double tmp;
		int i,line;
		char cstr[100];
		Tokenize(format,tokens," ");
		if(infile.fail())
			throw CException("Unable to open file");
		infile.getline(cstr,99);
		istringstream istr((string) cstr);
		for(i=0;i<tokens.size();i++)
		{
			istr >>tmp;
			if(istr.fail())
			{
				title=(string) cstr;
				break;
			}
			else
			{
				data.push_back(tmp);
			}
		}
		if(data.size()==tokens.size())
			put(tokens,data);
		line=1;
		while(infile.getline(cstr,99))
		{
			vector<double> data2; 
			istringstream istr2((string) cstr);
			line++;
			for(i=0;i<tokens.size();i++)
			{
				istr2 >>tmp;
				if(istr2.fail())
				{
					ostringstream ostr;
					ostr <<"Bad file format in line:" <<line <<" col:" <<i+1;
					throw CException(ostr.str());
				}
				else
				{
					data2.push_back(tmp);
				}
			}
			put(tokens,data2);
		}
	}
	catch(CException e)
	{
		e.Report();
	}
}
void Creflectionset::readhkl(istream &infile)
{
	read(infile,"h k l a p f");
}
void Creflectionset::readhk(istream &infile)
{
	read(infile,"h k a p f");
}
void Creflectionset::readaph(istream &infile)
{
	read(infile,"h k a p");
}
void Creflectionset::write(ostream &outfile,string format)
{
	try
	{
    	vector<string> tokens; 
		Tokenize(format,tokens," ");
		string bufstr="";
		char buf[20];
		int h,k,l,dim,i;
		dim=min(fom.getdim(),min(amp.getdim(),pha.getdim()));
		if(outfile.fail())
			throw CException("Unable to open file");
		if(title!="")
			outfile << title <<endl;
		for(h=0;h<=(int)dim;h++)
		{
			for(k=-(int)dim;k<=(int)dim;k++)
			{
				for(l=-(int)dim;l<=(int)dim;l++)
				{
					if(amp(h,k,l) > 0 && (h>0 || k>0))
					{
						bufstr="";
						for(i=0;i<tokens.size();i++)
						{
								getasstring(buf,h,k,l,tokens[i]);
								bufstr+=buf;
						}
						outfile <<bufstr <<endl;
					}
				}
			}
		}
	}
	catch(CException e)
	{
		e.Report();
	}
}
void Creflectionset::writehkl(ostream &outfile)
{
	write(outfile,"h k l a p f");

}
void Creflectionset::writehk(ostream &outfile)
{
	write(outfile,"h k a p f");
}
void Creflectionset::writeaph(ostream &outfile)
{
	try
	{
		int h,k,l,dim;
		dim=min(fom.getdim(),min(amp.getdim(),pha.getdim()));
		char tstr[100];
		if(outfile.fail())
			throw CException("Unable to open file");
		if(title!="")
			outfile << title <<endl;
		for(h=0;h<=(int)dim;h++)
		{
			for(k=-(int)dim;k<=(int)dim;k++)
			{
				l=0;
					if(amp(h,k,l) > 0 && (h>0 || k>0))
					{
						sprintf(tstr,"%8d%8d%16.1f%16.1f%8d\n",h,k,amp(h,k,l),pha(h,k,l),1);
						outfile <<tstr;
					}
			}
		}
	}
	catch(CException e)
	{
		e.Report();
	}
}
string Creflectionset::gettitle(void)
{
	return title;
}
string Creflectionset::puttitle(string t)
{
	title=t;
}
void Creflectionset::permutate(int cw)
{
	try
	{
		int h,k,l;
		int dim;
		Creflectionsarray tmp;
		tmp=amp;
		dim=(int)amp.getdim();
		if(cw==1)
		{
			for(h=0;h<=dim;h++)
				for(k=-dim;k<=dim;k++)
					for(l=-dim;l<=dim;l++)
					{
						amp.put(h,k,l,tmp.get(l,h,k));
					}
			tmp=fom;
			dim=(int)fom.getdim();
			for(h=0;h<=dim;h++)
				for(k=-dim;k<=dim;k++)
					for(l=-dim;l<=dim;l++)
						fom(h,k,l)=tmp.get(l,h,k);

			Cphasearray ptmp=pha;
			dim=(int)pha.getdim();
			for(h=0;h<=dim;h++)
				for(k=-dim;k<=dim;k++)
					for(l=-dim;l<=dim;l++)
						pha(h,k,l)=ptmp.get(l,h,k);
		}
		else if(cw==-1)
		{
			for(h=0;h<=dim;h++)
				for(k=-dim;k<=dim;k++)
					for(l=-dim;l<=dim;l++)
						amp(h,k,l)=tmp.get(k,l,h);
			tmp=fom;
			dim=(int)fom.getdim();
			for(h=0;h<=dim;h++)
				for(k=-dim;k<=dim;k++)
					for(l=-dim;l<=dim;l++)
						fom(h,k,l)=tmp.get(k,l,h);

			Cphasearray ptmp=pha;
			dim=(int)pha.getdim();
			for(h=0;h<=dim;h++)
				for(k=-dim;k<=dim;k++)
					for(l=-dim;l<=dim;l++)
						pha(h,k,l)=ptmp.get(k,l,h);
		}
		else
			throw CException("No valid permutation\n");
	}
	catch(CException e)
	{
		e.Report();
	}

}

void Creflectionset::rotate(char axis,int num)
{
	amp.rotate(axis,num);
	pha.rotate(axis,num);
	fom.rotate(axis,num);
}
void Tokenize(const string& str,
                      vector<string>& tokens,
                      const string& delimiters)
{
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

