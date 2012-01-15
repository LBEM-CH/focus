#include <map>
#include <vector>
#include <fstream>
#include <iostream>
#include <float.h>
#include <math.h>
using namespace std;

#define DEBUG 0
#define pi 3.14159265


struct fltCmp{
  bool operator()(const float &a, const float &b) const
  {
    return a<b;
  }
};

struct intVectCmp{
  bool operator()(vector<int> const &a, vector<int> const &b) const
  {
    if(a.at(0)<b.at(0)) return true;
    else return a.at(1)<b.at(1);
  }
};

#define vectorF vector<float>
#define vectorPair pair <vector<float>*, vector<float>* >
#define vectorList map<int,vectorF*>
#define latticeVectors map<float,vector<float>*>
#define latticeIntVectors map<float,vector<int>*,fltCmp>
#define latticePairs map<vector<int>,vectorF,intVectCmp>


float mag(vectorF *v)
{
  return sqrtf(v->at(0)*v->at(0)+v->at(1)*v->at(1));
}

float vLength(float a, float b)
{
  return sqrtf(a*a+b*b);
}

vectorList peakList;
vectorList latticeCandidates;
latticeIntVectors latticeResults;
latticePairs millerVectorPairs;

double nodesPerPeak(float *l, vectorList &peakList)
{
  vectorList::iterator it;
  double maxRadius = 0.0, radius;
  double avgV[2] = {0.0, 0.0};;
  for(int i=0;i<peakList.size();i++)
  {
    avgV[0]+=peakList[i]->at(0); avgV[1]+=peakList[i]->at(1);
    //radius = vLength(it->second.at(0),it->second.at(1));
    //if(radius>maxRadius) maxRadius = radius;
  }
  avgV[0]/=peakList.size(); avgV[1]/=peakList.size();
  for(int i=0;i<peakList.size();i++)
  {
    radius = vLength(peakList[i]->at(0)-avgV[0],peakList[i]->at(1)-avgV[1]);
    if(radius>maxRadius) maxRadius = radius;
  }

  double det = fabs(l[0]*l[3]-l[2]*l[1]);
  return (3.14159256*maxRadius*maxRadius)/(peakList.size()*det);
}


int load(char *filename)
{
  vector<float> *v;
  float x,y,z;
  int i=0;

  ifstream f;
  f.open(filename);
  if(!f.is_open()) return 0;
  while(!f.eof())
  {
    f>>x>>y>>z;
    if(x>=0.0)
    {
      v = new vector<float>;
      v->push_back(x); v->push_back(y); v->push_back(z);
      peakList[i] = v;
      i++;
    }
  }
  if(i <= 2) {cout<<"Not enough peaks present for fit."<<endl<<endl<<"Results: "<<endl<<"0, 0, 0, 0"<<endl; return (0);}
  f.close();
  return 1;
}

void clear()
{
  for(int i=0;i<peakList.size();i++)
    delete peakList[i];

  while(!latticeResults.empty())
  {
    delete latticeResults.begin()->second;
    latticeResults.erase(latticeResults.begin());
  }

}

float r2(vectorF *v)
{
  return v->at(0)*v->at(0)+v->at(1)*v->at(1);
}

float maxRadiusOf(int k)
{
  float rMax = 0.0;
  float r = 0.0;
  for(int i=0;i<k;i++)
  {
    r = r2(peakList[i]); 
    if(r>rMax) rMax = r;
  }

  return rMax;
} 

void findLatticeCandidates(float rMax, float threshold = 0.0)
{
  int k = 0;
  for(int i=0;i<peakList.size();i++)
    if(peakList[i]->at(2)>threshold && r2(peakList[i])<rMax)
    {
      latticeCandidates[k] = peakList[i];
      k++;
    }
}

float latticeError(vectorF *l1, vectorF *l2)
{
  float l[2][2];
  float invL[2][2];
  float mErr, nErr;
  float error = 0.0;
  float det;

  l[0][0]=l1->at(0);
  l[1][0]=l1->at(1);

  l[0][1]=l2->at(0);
  l[1][1]=l2->at(1); 

  float t[4] = {l1->at(0),l1->at(1),l2->at(0),l2->at(1)};
  
  det = l[0][0]*l[1][1]-l[0][1]*l[1][0];

  if(det==0.0) return FLT_MAX;

  invL[0][0] = l[1][1]/det;
  invL[1][0] = -l[1][0]/det;
  invL[0][1] = -l[0][1]/det;
  invL[1][1] = l[0][0]/det;

  float angle=fabs(acos((l1->at(0)*l2->at(0)+l1->at(1)*l2->at(1))/(mag(l1)*mag(l2))))*180.0/pi;

  for(int i=0;i<peakList.size();i++)
  {
    mErr = 1.0 - fabs(2.0*fabs(fmodf(peakList[i]->at(0)*invL[0][0]+peakList[i]->at(1)*invL[0][1],1.0)) - 1.0);
    nErr = 1.0 - fabs(2.0*fabs(fmodf(peakList[i]->at(0)*invL[1][0]+peakList[i]->at(1)*invL[1][1],1.0)) - 1.0);
    if(angle>10.0)
      error+=(mErr*mErr + nErr*nErr)*peakList[i]->at(2);
    else
      error+=(mErr*mErr + nErr*nErr)*peakList[i]->at(2) + exp(10-angle);
  
  }
 /* float scale = nodesPerPeak(t,peakList);
  if(scale<1.0) scale=(1.0-scale)*100.0;
  scale/=2.0;
  error+=scale;
 */ 
  if (DEBUG) cout<<" <"<<l1->at(0)<<", "<<l1->at(1)<<"> <"<<l2->at(0)<<", "<<l2->at(1)<<"> "<<angle<<" "<<error<<endl;
  return error;
}

void findLatticeFitness()
{
  vector<int> *v;
	float t[4];

  for(int i=0;i<latticeCandidates.size();i++)
    for(int j = i + 1; j<latticeCandidates.size();j++)
    {
      v = new vector<int>;
      v->push_back(i); v->push_back(j);
      latticeResults[latticeError(latticeCandidates[i],latticeCandidates[j])] = v;
    }
}

void rectify(vectorF *v)
{
  if(v->at(0)<0.0)
  {
    (*v)[0]=-v->at(0);
    (*v)[1]=-v->at(1);
  }
}


float scalarProduct(vectorF *& l1, vectorF *& l2)
{
	float ml1 = mag(l1);
	float ml2 = mag(l2);
    float theta = acos((l1->at(0) * l2->at(0) + l1->at(1) * l2->at(1)) / (ml1 * ml2));
    return theta;
}

bool validateCandidateVectors(vectorList& vectorCandidates, vectorPair& lattice)
{
	if(vectorCandidates.size() < 2)
		return false;

	//vectorPair lattice;
	//the shortest vector should certainly be a candidate
	vectorF* l1 = vectorCandidates.begin()->second;
	if(DEBUG) cout<<"C1: "<<l1->at(0)<<" "<<l1->at(1)<<" "<< "length" <<mag(l1)<<endl;


	//delete it from the candidate list
	vectorCandidates.erase(vectorCandidates.begin());


	  vectorList::iterator it;
	  for(it = vectorCandidates.begin(); it!=vectorCandidates.end(); it++)
	  {
		vectorF* l2 = it->second;
		float theta = scalarProduct(l1, l2);
		if(DEBUG) cout<<"C: "<<l2->at(0)<<" "<<l2->at(1)<<" "<< it->first <<" length " <<theta*180.0/pi <<" degrees." <<endl;
		lattice = make_pair(l1,l2);

		if(theta < pi/2.0)
		{
			//lattice = make_pair(l1,l2);
			//switch vectors
			if(theta < 0.0)
			{
				vectorF* t = lattice.first;
				lattice.first = lattice.second;
				lattice.second = t;
			}
			if(l2->at(0) < 0.0)
				return false;

			return true;
		}

	  }
	  return true;
}

int reduceVectors(vectorF *l1, vectorF *l2)
{
  rectify(l1);
  rectify(l2);

  float ml1 = mag(l1);
  float ml2 = mag(l2);
  vectorF s;
  float delta = 1.0;

  if(DEBUG) cout<<"Reducing Vectors: "<<endl;
  if(DEBUG) cout<<"L1: "<<l1->at(0)<<" "<<l1->at(1)<<" "<<ml1<<endl;
  if(DEBUG) cout<<"L2: "<<l2->at(0)<<" "<<l2->at(1)<<" "<<ml2<<endl;


  float theta = scalarProduct(l1, l2);
  if(DEBUG) cout<<"Theta: "<<theta<<" "<<theta*180.0/pi<<" degrees."<<endl;
  if(theta<pi/2.0)
  {
    if(ml1>ml2)
    {
      s.push_back(l1->at(0)-l2->at(0));
      s.push_back(l1->at(1)-l2->at(1));
    }
    else
    {
      s.push_back(l2->at(0)-l1->at(0));
      s.push_back(l2->at(1)-l1->at(1));
    }
  }
  else
  {
    s.push_back(l1->at(0)+l2->at(0));
    s.push_back(l1->at(1)+l2->at(1));
  }

  if(DEBUG) cout<<"S: "<<s[0]<<" "<<s[1]<<" "<<mag(&s)<<endl;
  
  if(ml1>ml2)
  {
    if(mag(&s)<ml1)
    {
      *l1 = s;
      return 1;
    }
    // if both vectors have the same length (within delta) favor the one that
    // spans the smaller angle
    else if(fabs(mag(&s)-ml1)<delta && theta > pi/2.0)
    {
    	*l1 = s;
    	return 0;
    }
  }
  else 
  {
    if(mag(&s)<ml2)
    {
      *l2 = s;
      return 1;
    }
    // if both vectors have the same length (within delta) favor the one that
    // spans the smaller angle
    else if(fabs(mag(&s)-ml2)<delta && theta > pi/2.0)
    {
       *l2 = s;
       return 0;
    }
  }
  
  return 0;
}

void getCandidateVectors(vectorList& vectorCandidates, vectorPair lattice)
{
	vectorCandidates.clear();
	vectorF * l1 = lattice.first;
	vectorF * l2 = lattice.second;
	vectorF* c;
	vectorF* d;

	//add the original vectors to the candidate list
	vectorCandidates[mag(l1)]=l1;
	vectorCandidates[mag(l2)]=l2;
	float theta = scalarProduct(l1, l2);
	if(DEBUG) cout<<"Theta: "<<theta<<" "<<theta*180.0/pi<<" degrees."<<endl;
	if(theta<pi/2.0)
	{
		c = new vector<float>();
		c->push_back(l1->at(0)-l2->at(0));
		c->push_back(l1->at(1)-l2->at(1));
		vectorCandidates[mag(c)] = c;

		d = new vector<float>();
		d->push_back(l2->at(0)-l1->at(0));
		d->push_back(l2->at(1)-l1->at(1));
		vectorCandidates[mag(d)] = d;
	}
	else
	{
		c = new vector<float>();
		c->push_back(l1->at(0)+l2->at(0));
		c->push_back(l1->at(1)+l2->at(1));
		vectorCandidates[mag(c)] = c;

	}
}

bool sameLattices(vectorPair lattice, vectorPair prev)
{
	if(lattice.first->at(0)==prev.first->at(0) && lattice.first->at(1)==prev.first->at(1) && lattice.second->at(0)==prev.second->at(0) && lattice.second->at(1)==prev.second->at(1))
		return true;
	return false;

}


int selectVectors(vectorF *l1, vectorF *l2)
{
  rectify(l1);
  rectify(l2);

  float ml1 = mag(l1);
  float ml2 = mag(l2);
  vectorF* c;
  vectorF* d;


  if(DEBUG) cout<<"Reducing Vectors: "<<endl;
  if(DEBUG) cout<<"L1: "<<l1->at(0)<<" "<<l1->at(1)<<" "<<ml1<<endl;
  if(DEBUG) cout<<"L2: "<<l2->at(0)<<" "<<l2->at(1)<<" "<<ml2<<endl;

  vectorList vectorCandidates;
  //the original vectors
  vectorPair lattice = make_pair(l1,l2);

  //calculate the candidate lattice vectors from the given lattice
  getCandidateVectors(vectorCandidates, lattice);

  vectorPair previousLattice = lattice;
  while(!validateCandidateVectors(vectorCandidates,lattice))
  {
	  if(sameLattices(lattice, previousLattice))break;
	  getCandidateVectors(vectorCandidates, lattice);
  }


  l1 = lattice.first;
  l2 = lattice.second;

//  else
//  {
//	  if(DEBUG) cout<<"Could not find enough candidates: "<<endl;
//  }



  if(DEBUG) cout<<"RESUTING LATTICE Vectors: "<<endl;
  if(DEBUG) cout<<"L1: "<<l1->at(0)<<" "<<l1->at(1)<<" "<<mag(l1)<<endl;
  if(DEBUG) cout<<"L2: "<<l2->at(0)<<" "<<l2->at(1)<<" "<<mag(l2)<<endl;



  return 0;
}




void cannonicalForm(vectorF *&l1, vectorF *&l2)
{
  float s = atan2(-l1->at(1),l1->at(0));
  float t = atan2(-l2->at(1),l2->at(0));
  

  if(s>t) 
  {
    vectorF *l3 = l2;
    l2 = l1;
    l1 = l3;
  }
}

void generatePairs(vectorF *l1, vectorF *l2)
{
  millerVectorPairs.clear();
  float invL[2][2];
  float det = l1->at(0)*l2->at(1)-l2->at(0)*l1->at(1);
  invL[0][0] = l2->at(1)/det;
  invL[1][0] = -l1->at(1)/det;
  invL[0][1] = -l2->at(0)/det;
  invL[1][1] = l1->at(0)/det;

  float t[4] = {l1->at(0),l1->at(1),l2->at(0),l2->at(1)};

  int m,n;
  float mx, ny;
  float mErr, nErr;
  float epsilon = (0.10)*(0.10) * 2.0;

  for(int i=0;i<peakList.size();i++)
  {
    vector<int> mI;
    vectorF v;

    mx = (peakList[i]->at(0)*invL[0][0]+peakList[i]->at(1)*invL[0][1]); 
    ny = (peakList[i]->at(0)*invL[1][0]+peakList[i]->at(1)*invL[1][1]);
    if(mx<0.0) mx-=0.5; else mx+=0.5;
    if(ny<0.0) ny-=0.5; else ny+=0.5;
    
    m = int(mx);
    n = int(ny);
    if(m!=0 || n!=0)
    {
      mI.push_back(m);
      mI.push_back(n);

      mErr = 1.0 - fabs(2.0*fabs(fmodf(peakList[i]->at(0)*invL[0][0]+peakList[i]->at(1)*invL[0][1],1.0)) - 1.0);
      nErr = 1.0 - fabs(2.0*fabs(fmodf(peakList[i]->at(0)*invL[1][0]+peakList[i]->at(1)*invL[1][1],1.0)) - 1.0);

      v.push_back(peakList[i]->at(0));
      v.push_back(peakList[i]->at(1));
      v.push_back((nErr*nErr+mErr*mErr));

      if(DEBUG) cout<<m<<" "<<n<<" "<<v.at(0)<<" "<<v.at(1)<<" "<<v.at(2)<<endl;

      if(v.at(2)<epsilon)
        if(millerVectorPairs.find(mI)==millerVectorPairs.end())
          millerVectorPairs.insert(make_pair(mI,v));
        else if(v.at(2)<millerVectorPairs[mI].at(2))
          millerVectorPairs[mI] = v;
    }
  }
}

void calculateLattice(vectorF *&l1, vectorF *&l2)
{
  double smx2=0.0, smy2=0.0, smxmy=0.0, smxrx=0.0, smyrx=0.0, smxry=0.0, smyry=0.0;
  double det=0.0;
  double c1 = 0.0, c2 = 0.0;

  latticePairs::iterator it;
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {  
    double mx = double(it->first.at(0)), my = double(it->first.at(1)), rx = it->second.at(0), ry = it->second.at(1);
    smx2+=mx*mx;
    smy2+=my*my;
    smxmy+=mx*my;
    smxrx+=mx*rx;
    smyrx+=my*rx;
    smxry+=mx*ry;
    smyry+=my*ry;
  }

  det = smx2*smy2-smxmy*smxmy;
  if(det!=0.0)
  {
    c1 = (smx2*smyrx-smxmy*smxrx)/det;
    c2 = (smx2*smyry-smxmy*smxry)/det;

    (*l1)[0]=smxrx/smx2 - c1*smxmy/smx2;
    (*l1)[1]=smxry/smx2 - c2*smxmy/smx2;

    (*l2)[0]=c1;
    (*l2)[1]=c2;
  } 

}

void printPeakList()
{
  for(int i=0;i<peakList.size();i++)
     cout<<peakList[i]->at(0)<<" "<<peakList[i]->at(1)<<" "<<peakList[i]->at(2)<<endl;
}

void printCandidateList()
{
  cout<<endl<<"Candidates: "<<endl;
  for(int i=0;i<latticeCandidates.size();i++)
    cout<<i<<" "<<latticeCandidates[i]->at(0)<<" "<<latticeCandidates[i]->at(1)<<" "<<latticeCandidates[i]->at(2)<<endl;
}

void printResultsList()
{
  int k = 0;
  latticeIntVectors::iterator it;
  for(it = latticeResults.begin(); it!=latticeResults.end() && k<4; it++)
  {
    cout<<latticeCandidates[it->second->at(0)]->at(0)<<" "<<-latticeCandidates[it->second->at(0)]->at(1)<<" ";
    cout<<latticeCandidates[it->second->at(1)]->at(0)<<" "<<-latticeCandidates[it->second->at(1)]->at(1)<<" "<<it->first<<endl;
    k++;
  }
}

void printMillerVectorPairs()
{
  latticePairs::iterator it;
  int k = 0;
  for(it = millerVectorPairs.begin(); it!=millerVectorPairs.end(); it++)
  {
    cout<<k<<": "<<it->first.at(0)<<", "<<it->first.at(1)<<" "<<it->second.at(0)<<", "<<it->second.at(1)<<" "<<it->second.at(2)<<endl;
    k++;
  }  
}

void printResult(vectorList &l)
{
  cout<<endl<<"Results: "<<endl;
  cout<<l[1]->at(0)<<", "<<l[1]->at(1)<<", ";
  cout<<l[0]->at(0)<<", "<<l[0]->at(1)<<" ";
  if(DEBUG) cout<<latticeResults.begin()->first<<endl;
  else cout<<endl;
}

int main(int argc, char **argv)
{
  if(argc!=2)
  {
    cout<<"No peak list given, assuming peaks_xy_final.dat"<<endl;
    char fileName[] = "peaks_xy_final.dat";
    if(!load(fileName)) {cout<<"peaks_xy_final.dat not present."<<endl; return (0);}
  }
  else
    if(!load(argv[1])) {cout<<argv[1]<<" not present."<<endl; return (0);}

  findLatticeCandidates(maxRadiusOf(8),0.0);

  findLatticeFitness();
  if (DEBUG) printCandidateList();


  vectorList l;
  l[0]=latticeCandidates[latticeResults.begin()->second->at(0)];
  l[1]=latticeCandidates[latticeResults.begin()->second->at(1)];

  while(reduceVectors(l[0],l[1]));
  cannonicalForm(l[0],l[1]);

//  while(selectVectors(l[0],l[1]));
//  cannonicalForm(l[0],l[1]);

  if(DEBUG) printResult(l);
  generatePairs(l[0],l[1]);
  if(DEBUG) printMillerVectorPairs();
  calculateLattice(l[0],l[1]);

  float t[4] = {0.0,0.0,0.0,0.0};
  int k = 0;
  while(sqrtf((t[0]-l[0]->at(0))*(t[0]-l[0]->at(0))+(t[1]-l[0]->at(1))*(t[1]-l[0]->at(1))+(t[2]-l[1]->at(0))*(t[2]-l[1]->at(0))+(t[3]-l[1]->at(1))*(t[3]-l[1]->at(1)))>0.0001)
  {
    t[0] = l[0]->at(0); t[1] = l[0]->at(1); t[2] = l[1]->at(0); t[3] = l[1]->at(1);
    if(DEBUG) printResult(l);
    generatePairs(l[0],l[1]);
    if(DEBUG) printMillerVectorPairs();
    calculateLattice(l[0],l[1]);
    k++;
  }
  cout<<"Converged in "<<k<<" steps."<<endl;
  printResult(l);
  clear();
  return 0;
}
