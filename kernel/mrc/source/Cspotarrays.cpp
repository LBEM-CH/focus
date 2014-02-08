#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Cspotarrays.h"

#define MAX(v1, v2) ((v1) > (v2)? (v1) : (v2))

Chkamparray::Chkamparray(void)
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			amp[i][j]=0;
		}
	title[0]=0;
}
Chkamparray::Chkamparray(char *file)
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			amp[i][j]=0;
		}
	readlst(file);
}
void Chkamparray::rmneg()
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			if(amp[i][j]<0)
				amp[i][j]=0;
		}
}
void Chkamparray::rmones()
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			if(amp[i][j]==1.0)
				amp[i][j]=0;
		}
}

double Chkamparray::getamp(int h, int k)
{
	return amp[h+maxdim/2][k+maxdim/2];
}
char Chkamparray::exists(int h, int k)
{
	if(amp[h+maxdim/2][k+maxdim/2]!=0)
		return 1;
	else 
		return 0;
}
void Chkamparray::putamp(int h, int k, double a)
{
	amp[h+maxdim/2][k+maxdim/2]=a;
}
void Chkamparray::readlst(char *file)
{
	int h,k;
	double a;
	FILE *fp;
	fp=fopen(file,"r");
	if ( fgets(title,100,fp) != NULL )
	{
		for(;;)
		{
			if ( fscanf(fp,"%d %d %lf",&h,&k,&a) != EOF )
			{
				if(h==100)
					break;
				amp[h+maxdim/2][k+maxdim/2]=a;		
				amp[-h+maxdim/2][-k+maxdim/2]=a;
			}
			else
			{
				perror ("Error fscan failed");
			}
		}
		fclose(fp);
	}
	else
	{
		perror ("Error reading file");
	}
}
void Chkamparray::readp4lst(char *file)
{
	int h,k;
	double a;
	FILE *fp;
	fp=fopen(file,"r");
	if ( fgets(title,100,fp) != NULL )
	{
		for(;;)
		{
			if ( fscanf(fp,"%d %d %lf",&h,&k,&a) != EOF )
			{
				if(h==100)
					break;
				amp[h+maxdim/2][k+maxdim/2]=a;		
				amp[-h+maxdim/2][-k+maxdim/2]=a;		
				amp[-k+maxdim/2][h+maxdim/2]=a;		
				amp[k+maxdim/2][-h+maxdim/2]=a;
			}
			else
			{
				perror ("Error fscan failed");
			}
		}
		fclose(fp);
	}
	else
	{
		perror ("Error reading file");
	}
}
void Chkamparray::writelst(char *file)
{
	int h,k;
	double a;
	FILE *fp;
	fp=fopen(file,"w");
	fprintf(fp,"%s\n",title);
	for(h=-maxdim/2;h<maxdim/2;h++)
	{
		for(k=-maxdim/2;k<maxdim/2;k++)
		{
			a=amp[h+maxdim/2][k+maxdim/2];
			if(a!=0)
				fprintf(fp,"%4d %4d %6f\n",h,k,a);
		}
	}
	fprintf(fp,"%4d %4d %6d\n",100,100,100);
	fclose(fp);
}
void Chkamparray::gettitle(char *t)
{
	strcpy(t,title);
}
void Chkamparray::puttitle(char *t)
{
	strcpy(title,t);
}
void Chkamparray::makepx(char num)
{
	int h,k,c=0;
	double a;
	switch(num)
	{
	case 4:
		for(h=0;h<=maxdim/2;h++)
		{
			for(k=0;k<=maxdim/2;k++)
			{
				a=getamp(h,k)+getamp(-k,h)+getamp(-h,-k)+getamp(k,-h);
				c=exists(h,k)+exists(-k,h)+exists(-h,-k)+exists(k,-h);
				if(c>0)
					a=a/c;
				putamp(h,k,a);
				putamp(-h,-k,a);
				putamp(-k,h,a);
				putamp(k,-h,a);
			}
		}
		break;
	}
}
void Chkamparray::revhk()
{
	int h,k;
	double tmp[maxdim][maxdim];
	for(h=0;h<maxdim;h++)
	{
		for(k=0;k<maxdim;k++)
		{
			tmp[h][k]=amp[h][k];
		}
	}
	for(h=0;h<maxdim;h++)
	{
		for(k=0;k<maxdim;k++)
		{
			amp[h][k]=tmp[k][h];
		}
	}
}

Chkamparray& Chkamparray::operator=(Chkamparray tmp)
{
	int h,k;
	for(h=-maxdim/2;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			putamp(h,k,tmp.getamp(h,k));
		}
	}
	return *this;
}
Chkamparray Chkamparray::operator+(Chkamparray tmp)
{
	int h,k;
	Chkamparray ntmp;
	for(h=-maxdim/2;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			ntmp.putamp(h,k,tmp.getamp(h,k)+getamp(h,k));
		}
	}
	return ntmp;
}
Chkamparray Chkamparray::operator*(double n)
{
	int h,k;
	Chkamparray ntmp;
	for(h=-maxdim/2;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			ntmp.putamp(h,k,n*getamp(h,k));
		}
	}
	return ntmp;
}
//*************************************************************************Chkampsigarray*******************************


Chkampsigarray::Chkampsigarray(void)
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			sig[i][j]=0;
		}
}
Chkampsigarray::Chkampsigarray(char *file)
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			sig[i][j]=0;
		}
	readlst(file);
}
double Chkampsigarray::getsig(int h, int k)
{
	return sig[h+maxdim/2][k+maxdim/2];
}
void Chkampsigarray::putsig(int h, int k, double s)
{
	sig[h+maxdim/2][k+maxdim/2]=s;
}
void Chkampsigarray::readlst(char *file)
{
	int h,k;
	double a,s;
	char t[100];
	FILE *fp;
	fp=fopen(file,"r");

	if ( fgets(t,100,fp) != NULL )
	{
		Chkamparray::puttitle(t);
		while(fscanf(fp,"%d %d %lf %lf",&h,&k,&a,&s)!=EOF)
		{
			if(h==100)
				break;
			Chkamparray::putamp(h,k,a);		
			Chkamparray::putamp(-h,-k,a);		
			sig[h+maxdim/2][k+maxdim/2]=s;		
			sig[-h+maxdim/2][-k+maxdim/2]=s;		
		}
		fclose(fp);
	}
	else
	{
		perror ("Error reading file");
	}
}
void Chkampsigarray::readp4lst(char *file)
{
	int h,k;
	double a,s;
	char t[100];
	FILE *fp;
	fp=fopen(file,"r");
	
	if ( fgets(t,100,fp) != NULL )
	{
		Chkamparray::puttitle(t);
		while(fscanf(fp,"%d %d %lf %lf",&h,&k,&a,&s)!=EOF)
		{
			if(h==100)
				break;
			Chkamparray::putamp(h,k,a);		
			Chkamparray::putamp(-h,-k,a);		
			Chkamparray::putamp(-k,h,a);		
			Chkamparray::putamp(k,-h,a);		
			sig[h+maxdim/2][k+maxdim/2]=s;		
			sig[-h+maxdim/2][-k+maxdim/2]=s;		
			sig[-k+maxdim/2][h+maxdim/2]=s;		
			sig[k+maxdim/2][-h+maxdim/2]=s;		
		}
		fclose(fp);
	}
	else
	{
		perror ("Error reading file");
	}
}
void Chkampsigarray::writelst(char *file)
{
	int h,k;
	double a,s;
	char t[100];
	FILE *fp;
	fp=fopen(file,"w");
	Chkamparray::gettitle(t);
	fprintf(fp,"%s\n",t);
	for(h=0;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			a=Chkamparray::getamp(h,k);
			s=getsig(h,k);
			if(a!=0 && (h!=0 || k>=0))
				fprintf(fp,"%4d %4d %10.2f %10.2f \n",h,k,a,s);
		}
	}
	fprintf(fp,"%4d %4d %6d %6d \n",100,100,100,100);
	fclose(fp);
}
void Chkampsigarray::makepx(char num)
{
	int h,k,c;
	double t,s,a;
	switch(num)
	{
	case 4:
		for(h=0;h<=maxdim/2;h++)
		{
			for(k=0;k<=maxdim/2;k++)
			{
				a=0;
				t=0;
				c=exists(h,k)+exists(-k,h)+exists(-h,-k)+exists(k,-h);
				if(exists(h,k))
				{
					a+=getamp(h,k)/(getsig(h,k)*getsig(h,k)+0.0000001);
					t+=1/(getsig(h,k)*getsig(h,k)+0.0000001);
				}	
				if(exists(-h,-k))
				{
					a+=getamp(-h,-k)/(getsig(-h,-k)*getsig(-h,-k)+0.0000001);
					t+=1/(getsig(-h,-k)*getsig(-h,-k)+0.0000001);
				}	
				if(exists(-k,h))
				{
					a+=getamp(-k,h)/(getsig(-k,h)*getsig(-k,h)+0.0000001);
					t+=1/(getsig(-k,h)*getsig(-k,h)+0.0000001);
				}	
				if(exists(k,-h))
				{
					a+=getamp(k,-h)/(getsig(k,-h)*getsig(k,-h)+0.0000001);
					t+=1/(getsig(k,-h)*getsig(k,-h)+0.0000001);
				}
				if(t!=0)
					a=a/t;
				putamp(h,k,a);
				putamp(-h,-k,a);
				putamp(-k,h,a);
				putamp(k,-h,a);
				s=getsig(h,k)+getsig(-k,h)+getsig(-h,-k)+getsig(k,-h);
				if(c>0)
					s=s/c;
				putsig(h,k,s);
				putsig(-h,-k,s);
				putsig(-k,h,s);
				putsig(k,-h,s);
			}
		}
		break;
	}
}
void Chkampsigarray::revhk()
{
	int h,k;
	double tmp[maxdim][maxdim];
	for(h=0;h<maxdim;h++)
	{
		for(k=0;k<maxdim;k++)
		{
			tmp[h][k]=sig[h][k];
		}
	}
	for(h=0;h<maxdim;h++)
	{
		for(k=0;k<maxdim;k++)
		{
			sig[h][k]=tmp[k][h];
		}
	}
	Chkamparray::revhk();
}
Chkampsigarray& Chkampsigarray::operator=(Chkampsigarray tmp)
{
	int h,k;
	for(h=-maxdim/2;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			putsig(h,k,tmp.getsig(h,k));
		}
	}
	Chkamparray::operator=(tmp);
	return *this;
}
Chkampsigarray& Chkampsigarray::operator=(Chkamparray tmp)
{
	int h,k;
	for(h=0;h<maxdim;h++)
	{
		for(k=0;k<maxdim;k++)
		{
			sig[h][k]=0;
		}
	}
	Chkamparray::operator=(tmp);
	return *this;
}
Chkampsigarray Chkampsigarray::operator+(Chkampsigarray tmp)
{
	int h,k;
	Chkampsigarray ntmp;
	ntmp=Chkamparray::operator+(tmp);
	for(h=-maxdim/2;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			ntmp.putsig(h,k,(tmp.getsig(h,k)+getsig(h,k))/2);
		}
	}
	return ntmp;
}
//*************************************************************************Chkampphafomarray*******************************


Chkampphafomarray::Chkampphafomarray(void)
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			pha[i][j]=0;
			fom[i][j]=0;
		}
}
Chkampphafomarray::Chkampphafomarray(char *file)
{
	int i,j;
	for (i=0;i<maxdim;i++)
		for (j=0;j<maxdim;j++)
		{
			pha[i][j]=0;
			fom[i][j]=0;
		}
	readlst(file);
}
double Chkampphafomarray::getpha(int h, int k)
{
	return pha[h+maxdim/2][k+maxdim/2];
}
void Chkampphafomarray::putpha(int h, int k, double p)
{
	pha[h+maxdim/2][k+maxdim/2]=p;
}
double Chkampphafomarray::getfom(int h, int k)
{
	return fom[h+maxdim/2][k+maxdim/2];
}
void Chkampphafomarray::putfom(int h, int k, double f)
{
	fom[h+maxdim/2][k+maxdim/2]=f;
}
void Chkampphafomarray::readlst(char *file)
{
	int h,k;
	double a,p,f;
	char t[100];
	FILE *fp;
	fp=fopen(file,"r");
	
	if ( fgets(t,100,fp) != NULL )
	{
		Chkamparray::puttitle(t);
		for(;;)
		{
			if ( fscanf(fp,"%d %d %lf %lf %lf",&h,&k,&a,&p,&f) != EOF )
			{
				if(h==100)
					break;
				Chkamparray::putamp(h,k,a);		
				Chkamparray::putamp(-h,-k,a);		
				pha[h+maxdim/2][k+maxdim/2]=p;		
				pha[-h+maxdim/2][-k+maxdim/2]=p;		
				fom[h+maxdim/2][k+maxdim/2]=f;	
				fom[-h+maxdim/2][-k+maxdim/2]=f;
			}
			else
			{
				perror ("Error fscan failed");
			}
		}
		fclose(fp);
	}
	else
	{
		perror ("Error reading file");
	}
}
void Chkampphafomarray::readp4lst(char *file)
{
	int h,k;
	double a,p,f;
	char t[100];
	FILE *fp;
	fp=fopen(file,"r");
	if ( fgets(t,100,fp) != NULL )
	{
		Chkamparray::puttitle(t);
		for(;;)
		{
			if ( fscanf(fp,"%d %d %lf %lf %lf",&h,&k,&a,&p,&f) != EOF )
			{
				if(h==100)
					break;
				Chkamparray::putamp(h,k,a);		
				Chkamparray::putamp(-h,-k,a);		
				Chkamparray::putamp(-k,h,a);		
				Chkamparray::putamp(k,-h,a);		
				pha[h+maxdim/2][k+maxdim/2]=p;		
				pha[-h+maxdim/2][-k+maxdim/2]=p;		
				pha[-k+maxdim/2][h+maxdim/2]=p;		
				pha[k+maxdim/2][-h+maxdim/2]=p;		
				fom[h+maxdim/2][k+maxdim/2]=f;		
				fom[-h+maxdim/2][-k+maxdim/2]=f;		
				fom[-k+maxdim/2][h+maxdim/2]=f;		
				fom[k+maxdim/2][-h+maxdim/2]=f;
			}
			else
			{
				perror ("Error fscan failed");
			}
		}
		fclose(fp);
	}
	else
	{
		perror ("Error reading file");
	}
}
void Chkampphafomarray::writelst(char *file)
{
	int h,k;
	double a;
	char t[100];
	FILE *fp;
	fp=fopen(file,"w");
	gettitle(t);
	fprintf(fp,"%s\n",t);
	for(h=0;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			a=Chkamparray::getamp(h,k);
			if(a!=0 && (h!=0 || k>=0))
				fprintf(fp,"%4d %4d %6f %6f %6f \n",h,k,a,getpha(h,k),getfom(h,k));
		}
	}
	fprintf(fp,"%4d %4d %6d %6d %6d\n",100,100,100,100,100);
	fclose(fp);
}
void Chkampphafomarray::makepx(char num)
{
	int h,k,c;
	double p,f;
	switch(num)
	{
	case 4:
		for(h=0;h<=maxdim/2;h++)
		{
			for(k=0;k<=maxdim/2;k++)
			{
				p=getpha(h,k)+getpha(-k,h)+getpha(-h,-k)+getpha(k,-h);
				c=exists(h,k)+exists(-k,h)+exists(-h,-k)+exists(k,-h);
				if(c>0)
					p=p/c;
				putpha(h,k,p);
				putpha(-h,-k,p);
				putpha(-k,-h,p);
				putpha(k,-h,p);
				f=getfom(h,k)+getfom(-k,h)+getfom(-h,-k)+getfom(k,-h);
				if(c>0)
					f=f/c;
				putfom(h,k,f);
				putfom(-h,-k,f);
				putfom(-k,-h,f);
				putfom(k,-h,f);
			}
		}
		Chkamparray::makepx(num);
		break;
	}
}


//*******************************getscalefactor**********************
double getscalefactor(Chkamparray a1,Chkamparray a2)
{
	double scale=0;
	int num=0,h,k;
	for(h=0;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			if(a1.exists(h,k) && a2.exists(h,k))
			{
				scale+=(a1.getamp(h,k)/a2.getamp(h,k));
				num++;
			}
		}
	}
	return scale/num;
}
double getrmerge(Chkamparray a1,Chkamparray a2)
{
	double rmerge=0;
	int num=0,h,k;
	for(h=0;h<=maxdim/2;h++)
	{
		for(k=-maxdim/2;k<=maxdim/2;k++)
		{
			if(a1.exists(h,k) && a2.exists(h,k))
			{
				rmerge+=fabs((a1.getamp(h,k)-a2.getamp(h,k)))/MAX(a1.getamp(h,k),a2.getamp(h,k));
				if(fabs((a1.getamp(h,k)-a2.getamp(h,k)))>2)
				num++;
			}
		}
	}
	return rmerge/num;
}
