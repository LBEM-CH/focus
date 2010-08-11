#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <map>
#include <vector>
#include <math.h>
using namespace std;

#define DEBUG 0

struct intVectCmp{
  bool operator()(const vector<int> &a, const vector<int> &b) const
  {
    if(a.at(0)<b.at(0)) return true;
    else return a.at(1)<b.at(1);
  }
};

double square(double x)
{
  return x*x;
}

#define vectorF vector<double>
#define vMap map<int,vectorF>
#define latticePairs map<vector<int>,vectorF,intVectCmp>

void split(string &text, string &delimiter, vector<string> &entries)
{
  int n = text.length();
  int a,b=0; 
  a = text.find_first_not_of(delimiter);
  while(b<n)
  {
    b = text.find_first_of(delimiter,a);
    if(b<0 || b>n) b = n;
    entries.push_back(text.substr(a,b-a));
    a = text.find_first_not_of(delimiter,b);
  }
}

double vLength(double x, double y)
{
  return sqrt(x*x+y*y);
}

void getLattice(char **argv, double *l)
{
  vector<string> results;
  string t = argv[1], d = ",";
  split(t,d,results);
  for(int i =0;i<4 && i<results.size();i++)
    l[i] = atof(results.at(i).c_str());
}

int getVectorList(char *fileName, vMap &v)
{
  ifstream f;
  f.open(fileName);
  if(!f.is_open()) {cout<<"Cannot open "<<fileName<<endl; return 0;}
  double x,y,z;
  int i = 0;
  while(!f.eof())
  {
    vectorF p;
    f>>x>>y>>z;
    if(DEBUG) cout<<x<<" "<<-y<<" "<<z<<endl;
    p.push_back(x); p.push_back(y); p.push_back(z);
    v[i]=p;
    i++;
  }
  f.close();
  return 1;
}

/*double latticeError(double *l, double x, double y, int m, int n)
{
  double e;
  e=vLength(l[0]*m+l[2]*n - x, l[1]*m+l[3]*n -y);
  double maxError = min(vLength(l[0]-l[2],l[1]-l[3]),vLength(l[0]+l[2],l[1]+l[3]))/2.0;
  cout<<x<<" "<<y<<" "<<m<<" "<<n<<"* "<<e<<" "<<maxError<<endl;
  return e;
}
*/

double latticeError(double *l, double mx, double ny, int m, int n)
{

  double e=vLength(l[0]*(mx-m)+l[2]*(ny-n),l[1]*(mx-m)+l[3]*(ny-n));
/*  double maxError;
  double theta = acos((l[0]*l[2]+l[1]*l[3])/(vLength(l[0],l[1])*vLength(l[2],l[3])))*180.0/(3.14159265);
  float a = vLength(l[0],l[1]), b = vLength(l[2],l[3]), c = vLength(l[0]-l[2],l[1]-l[3]), d = vLength(l[0]+l[2],l[1]+l[3]);
  if(fabs(theta)>90.0) 
    maxError = d*b*a/sqrt((d+b+a)*(b+a-d)*(a+d-b)*(d+b-a));
  else 
    maxError = a*b*c/sqrt((a+b+c)*(b+c-a)*(c+a-b)*(a+b-c));

  maxError = max(vLength(l[0]-l[2],l[1]-l[3]),vLength(l[0]+l[2],l[1]+l[3]))/2.0;
  if(true)
 {
    cout<<l[0]*(mx-m)+l[2]*(ny-n)<<"\t"<<l[1]*(mx-m)+l[2]*(ny-n)<<"\t";
    if(e>maxError) cout<<"!!--------->";  
    cout<<"Error: "<<e<<"\t"<<maxError<<"\t"<<theta<<endl;
  }
*/
  return e;
}

void generatePairs(double *l, vMap &peakList, latticePairs &millerVectorPairs)
{
  double invL[2][2];
  double det = l[0]*l[3]-l[2]*l[1];
  invL[0][0] = l[3]/det;
  invL[1][0] = -l[1]/det;
  invL[0][1] = -l[2]/det;
  invL[1][1] = l[0]/det;

  int m,n;
  double mx, ny;
  double mErr, nErr;
  double epsilon = 0.10 * max(vLength(l[0]-l[2],l[1]-l[3]),vLength(l[0]+l[2],l[1]+l[3]))/2.0;

  for(int i=0;i<peakList.size();i++)
  {
    vector<int> mI;
    vectorF v;

    mx = (peakList[i].at(0)*invL[0][0]+peakList[i].at(1)*invL[0][1]);
    ny = (peakList[i].at(0)*invL[1][0]+peakList[i].at(1)*invL[1][1]);
//    if(true)
//    {
//      cout.precision(4);
//      cout<<mx<<"\t"<<ny<<"\t";
//    }
//    if(mx<0.0) mx-=0.5; else mx+=0.5;
//    if(ny<0.0) ny-=0.5; else ny+=0.5;

    m = (int)rint(mx);
    n = (int)rint(ny);
//    if(true)
//      cout<<m<<"\t"<<n<<"\t*\t"<<peakList[i].at(0)<<"\t"<<peakList[i].at(1)<<"\t";
    if(m!=0 || n!=0)
    {
      mI.push_back(m);
      mI.push_back(n);

      //mErr = 1.0 - fabs(2.0*fabs(fmod(peakList[i].at(0)*invL[0][0]+peakList[i].at(1)*invL[0][1],1.0)) - 1.0);
      //nErr = 1.0 - fabs(2.0*fabs(fmod(peakList[i].at(0)*invL[1][0]+peakList[i].at(1)*invL[1][1],1.0)) - 1.0);

      mx = (peakList[i].at(0)*invL[0][0]+peakList[i].at(1)*invL[0][1]);
      ny = (peakList[i].at(0)*invL[1][0]+peakList[i].at(1)*invL[1][1]);

      v.push_back(peakList[i].at(0));
      v.push_back(peakList[i].at(1));
      //v.push_back(latticeError(l,peakList[i].at(0),peakList[i].at(1),m,n));
      v.push_back(latticeError(l,mx,ny,m,n));

      if(v.at(2)<epsilon)
        if(millerVectorPairs.find(mI)==millerVectorPairs.end())
          millerVectorPairs.insert(make_pair(mI,v));
        else if(v.at(2)<millerVectorPairs[mI].at(2))
          millerVectorPairs[mI] = v;
    }
  }
}

void printMillerVectorPairs(latticePairs &millerVectorPairs)
{
  latticePairs::iterator it;
  int k = 0;
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {
    cout<<k<<": "<<it->first.at(0)<<", "<<it->first.at(1)<<" "<<it->second.at(0)<<", "<<it->second.at(1)<<" "<<sqrt(it->second.at(2))<<" "<<sqrt(it->second.at(2)/2.0)*100.0<<"% of max error."<<endl;
    k++;
  }
}

void printError(double *l, latticePairs &millerVectorPairs)
{ 
  double absError = 0.0;

  latticePairs::iterator it;
  int k = 0;
  double maxError = max(vLength(l[0]-l[2],l[1]-l[3]),vLength(l[0]+l[2],l[1]+l[3]))/2.0;
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {
    absError+=it->second.at(2);
    k++;
  }
  //absError = sqrt(absError);
  cout<<":Absolute Error: "<<absError<<endl;
  cout<<"::RMSDn Error: "<<absError/(k*maxError)*100.0<<"%"<<endl;
  //if (DEBUG) cout<<":%Max Possible Error: "<<absError/(sqrt(2.0)*k)*100.0<<"% "<<endl;
  //cout<<"::Lattice Error: "<<absError/(sqrt(0.0225 * 2.0) * k)*100.0<<"%"<<endl;
}

void gaussJordanInverse(double *l,int n)
{ 
  double m[n][2*n];

  for(int i=0;i<n*n;i++)
  {
    m[i/n][i%n]=l[i];
    m[i/n][i%n+n] = double(i/n == i%n);
  }

  if(DEBUG)
  {
    for(int j=0;j<n;j++)
    {
      for(int i=0;i<2*n;i++)
      {
        cout.precision(3);
        cout<<m[j][i]<<"\t";
      }
      cout<<endl;
    }
    cout<<endl;
  }

  double scale;

  for(int i=0;i<n-1;i++)
  {
    for(int j=i+1;j<n;j++)
    {
      scale = (m[j][i]/m[i][i]);
      for(int k=i;k<2*n;k++)
        m[j][k]-=scale*m[i][k];
    }
  }

  for(int i=n-1;i>0;i--)
  {
    for(int j=i-1;j>=0;j--)
    {
      scale = (m[j][i]/m[i][i]);
      for(int k=i;k<2*n;k++)
        m[j][k]-=scale*m[i][k];
    }
  } 
  
  double pivot;

  for(int j=0;j<n;j++)
  {
    pivot = m[j][j];
    for(int k=j;k<2*n;k++)
      m[j][k]/=pivot;
  }

  if(DEBUG)
  {     
    for(int j=0;j<n;j++)
    {
      for(int i=0;i<2*n;i++)
      {
        cout.precision(2);
        cout<<m[j][i]<<"\t";
      }
      cout<<endl;
    }
  }

  for(int i=0;i<n*n;i++)
    l[i] = m[i/n][i%n+n];   

  if(DEBUG) cout<<endl;
}

double corrCoeff(int a, int b, double *l, latticePairs &millerVectorPairs)
{
  double x,y,avrX=0.0,avrY=0.0;
  double sum = 0.0;

  latticePairs::iterator it;
  int k = 0;
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {
    if(a==0)      avrX+=it->first.at(0);
    else if(a==1) avrX+=it->first.at(1);
    else if(a==2) avrX+=it->second.at(0);
    else if(a==3) avrX+=it->second.at(1);
    else if(a==4) avrX+=sqrt(it->second.at(2));

    if(b==0)      avrY+=it->first.at(0);
    else if(b==1) avrY+=it->first.at(1);
    else if(b==2) avrY+=it->second.at(0);
    else if(b==3) avrY+=it->second.at(1);
    else if(b==4) avrY+=sqrt(it->second.at(2));
    k++;
  }  
  avrX/=double(k);
  avrY/=double(k);


  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {
    if(a==0)      x=it->first.at(0);
    else if(a==1) x=it->first.at(1);
    else if(a==2) x=it->second.at(0);
    else if(a==3) x=it->second.at(1);
    else if(a==4) x=sqrt(it->second.at(2));

    if(b==0)      y=it->first.at(0);
    else if(b==1) y=it->first.at(1);
    else if(b==2) y=it->second.at(0);
    else if(b==3) y=it->second.at(1);
    else if(b==4) y=sqrt(it->second.at(2));
    sum+=x*y;
  } 
  sum -= double(k)*avrX*avrY;
  return sum;
}

void generateCorrelationMatrix(double *m, double *l, latticePairs &millerVectorPairs)
{
  double cc;
  double sxx[4];
  for(int i=0;i<4;i++)
  {
    sxx[i] = corrCoeff(i,i,l,millerVectorPairs);
    m[i+i*4] = 1.0;
  }

  for(int j=0;j<4;j++) 
    for(int i=j+1;i<4;i++)
    {
      cc = corrCoeff(j,i,l,millerVectorPairs); 
      m[i+j*4] = cc*cc/(sxx[j]*sxx[i]);
      m[j+i*4] = m[i+j*4];
    }
}

double cmDet(double *m, double *l, latticePairs &millerVectorPairs)
{
  double y[5], v[4] = {0.0,0.0,0.0,0.0};
  double r2 = 0.0;
  double sxx[5];
  double cc;

  for(int i=0;i<5;i++)
    sxx[i] = corrCoeff(i,i,l,millerVectorPairs);

  for(int i=0;i<5;i++)
  {
    cc = corrCoeff(i,4,l,millerVectorPairs);
    y[i] = cc*cc/(sxx[i]*sxx[4]);
  }

  for(int i=0;i<4;i++)
    for(int j=0;j<4;j++)
      v[i] += m[j+i*4]*y[j]; 

  for(int j=0;j<4;j++)
    r2+=v[j]*y[j];

  return r2;
}

double generalizedCorrCoeff(double *l, latticePairs &millerVectorPairs)
{

  double invL[2][2];
  double det = l[0]*l[3]-l[2]*l[1];
  invL[0][0] = l[3]/det;
  invL[1][0] = -l[1]/det;
  invL[0][1] = -l[2]/det;
  invL[1][1] = l[0]/det;
  double m,n;

  latticePairs::iterator it;

  double avrM[2]={0.0,0.0}, avrV[2]={0.0,0.0};
  double sxx=0.0, syy=0.0, sxy=0.0;

  int k = 0;
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {  
    m = invL[0][0]*it->second.at(0) + invL[0][1]*it->second.at(1); n=invL[1][0]*it->second.at(0) + invL[1][1]*it->second.at(1);
    //m = l[0]*it->first.at(0)+l[2]*it->first.at(1); n = l[1]*it->first.at(0)+l[3]*it->first.at(1);
    avrM[0]+=it->first.at(0); avrM[1]+=it->first.at(1);
    avrV[0]+=m; avrV[1]+=n;
    //avrV[0]+=it->second.at(0); avrV[1]+=it->second.at(1);  
    k++;
  }
  avrM[0]/=k; avrM[1]/=k;
  avrV[0]/=k; avrV[1]/=k;

  double cM[2], cV[2];
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {
    m = invL[0][0]*it->second.at(0) + invL[0][1]*it->second.at(1); n=invL[1][0]*it->second.at(0) + invL[1][1]*it->second.at(1);
    //m = l[0]*it->first.at(0)+l[2]*it->first.at(1); n = l[1]*it->first.at(0)+l[3]*it->first.at(1);
    cM[0] = it->first.at(0)-avrM[0]; cM[1] = it->first.at(1)-avrM[1]; 
    cV[0] = m - avrV[0]; cV[1] = n - avrV[1];
    //cV[0] = it->second.at(0)-avrV[0]; cV[1] = it->second.at(1)-avrV[1]; 
    sxx += cM[0]*cM[0] + cM[1]*cM[1];
    syy += cV[0]*cV[0] + cV[1]*cV[1];
    sxy += cM[0]*cV[0] + cM[1]*cV[1];
  }
  return sxy*sxy/(sxx*syy); 
}

double nodesPerPeak(double *l, latticePairs &pairs)
{
  latticePairs::iterator it;
  double maxRadius = 0.0, radius;
  double avgV[2] = {0.0, 0.0};;
  for(it = pairs.begin(); it!=pairs.end(); it++)
  {
    avgV[0]+=it->second.at(0); avgV[1]+=it->second.at(1);
    //radius = vLength(it->second.at(0),it->second.at(1));
    //if(radius>maxRadius) maxRadius = radius;
  }
  avgV[0]/=pairs.size(); avgV[1]/=pairs.size();
  for(it = pairs.begin(); it!=pairs.end(); it++)
  {
    radius = vLength(it->second.at(0)-avgV[0],it->second.at(1)-avgV[1]);
    if(radius>maxRadius) maxRadius = radius;
  }
 
  double det = fabs(l[0]*l[3]-l[2]*l[1]);
  return (3.14159256*maxRadius*maxRadius)/(pairs.size()*det);
}

int main(int argc, char **argv)
{
  cout<<endl;
  if(argc!=3) {cout<<"2dx_laterror.exe reduces a peak list to those points which fall closest to the given lattice.\nAt most one peak is assigned to a given Miller Index. \n Usage: 2dx_laterror {lattice} {filename}\n Lattice must be in form of u1,u2,v1,v2\n"<<endl; return 0;}
  double lattice[4];
  vMap peakList;
  latticePairs millerVectorPairs;  
  getLattice(argv,lattice);
  cout<<":Input Lattice: ";
  for(int i=0;i<4;i++)
    cout<<lattice[i]<<" ";
  cout<<endl;
  getVectorList(argv[2],peakList);
  generatePairs(lattice,peakList,millerVectorPairs);
  if(DEBUG) printMillerVectorPairs(millerVectorPairs);
  if(DEBUG) cout<<peakList.size()<<" "<<millerVectorPairs.size()<<endl;
  printError(lattice,millerVectorPairs);
  cout<<"::Peaks Used: "<<millerVectorPairs.size()<<" of "<<peakList.size()-1<<endl;
  cout<<"::Nodes/Peak Density: "<<nodesPerPeak(lattice,millerVectorPairs)<<endl;

  double m[4*4];
  generateCorrelationMatrix(m,lattice,millerVectorPairs);
  gaussJordanInverse(m,4);
  if (DEBUG) cout<<"::Coefficient of multiple determination for system: "<<cmDet(m,lattice,millerVectorPairs)<<endl;
  if (DEBUG) cout<<"::Generalized Correlation Coefficient: "<<generalizedCorrCoeff(lattice,millerVectorPairs)<<endl;
 
  return 0;
}
