/* 2dx_tiltgeom.cc
*
*  tilt geometry calculation from n points with given defocus
*
*/


#include <fstream>
#include <vector>
#include <string>
#include <cstring>
#include <complex>
#include <math.h>
#include <stdlib.h>
#include "mygeometry.h"
using namespace std;


//---------------------------------------------------------------------------------------
main()
{
	char  temp[1000],buffer[1000];
        char name[200];
	double stepsize;
	Cvector<double> tmp(3),w(3);
	vector<vector<double> > results;
	vector<Cvector<double> *> data;
	Cvector<double> center(3);
	int magnification, tilenumber;
// data aquisition
        cout << endl << "2dx_tiltgeom.exe: calculates tilt geometry from various defoci" << endl << endl;
        cout << "Name for outputfile: ";
        cin.getline(name,99);
        cout << name << endl;
	cout << "stepsize..........: ";
	cin.getline(temp,99);
	stepsize=atof(temp);
	cout << stepsize << endl;
	cout << "Magnification.....: ";
	cin.getline(temp,99);
	magnification=atoi(temp);
	cout << "Number of tiles...: ";
	cin.getline(temp,99);
	tilenumber=atoi(temp);
	cout << tilenumber << endl;
	double factor=stepsize/(double)magnification*10000;

	cout << "factor = " << factor << endl;

	cin.getline(temp,99);
        ifstream infile(temp);
	if ( !infile.is_open() ) { cerr << "Could not open file: " << temp << endl; return false; }
	infile.getline(temp,300);
	for(int i=0;i<tilenumber;i++)
	{
		for(int j=0;j<tilenumber;j++)
		{
			infile.getline(temp,300);
			char *tp,*tp2=temp;
			tp=strtok(&tp2[40]," ");
			if(tp!=NULL)
			{
				tmp[0]=atof(tp)*factor;
			}
			tp=strtok(&tp2[tilenumber*tilenumber]," ");
			if(tp!=NULL)
			{
				tmp[1]=atof(tp)*factor;
			}
			tp2=NULL;
			cout << tmp[0] << "," << tmp[1] << endl;
			Cvector<double> *tvec=new Cvector<double>(3);
			*tvec=tmp;
			// data is a 1D array of 3D vectors containing x,y,z
			data.push_back(tvec);
			center=center+tmp;
		}
	}
        infile.close();


	cout << endl << "input "<< tilenumber*tilenumber << "defocus values" << endl;;
	int k=0;
       
        for(int j=0;j<tilenumber;j++)
	{
	        cin.getline(temp,300);
                char *tp,*tp2=temp;
		for(int i=0;i<tilenumber;i++)
		{
			tp=strtok(tp2,",");
			if(tp!=NULL)
			{
				tmp[2]=atof(tp);
			}
			else
			{
				break;
			}
			tp2=NULL;
			(*data[k])[2]=tmp[2];
			k++;
		}
	}
	if(data.size()<3)
	{
		cerr <<" At least 3 points needed to calculate defocus"<<endl;
		exit(1);
	}
	cout << endl << "Number of defocus values read: " << data.size() << endl << endl;
        cout << "Xpos,Ypos ==> Defocus" << endl;
        for(int i=0;i<data.size();i++)
	{
		cout << (*data[i])[0] << "," << (*data[i])[1] << " ==> " << (*data[i])[2] << endl;
	}
        cout << endl;

	double m[4][3];

	for(int i=0;i<4;i++)
		for(int j=0;j<3;j++)
			m[i][j]=0;

	for(int i=0;i<data.size();i++)
	{
		m[0][0]+=(*data[i])[2];
		m[1][0]+=1;
		m[2][0]+=(*data[i])[0];
		m[3][0]+=(*data[i])[1];

		m[0][1]+=(*data[i])[2]*(*data[i])[0];
		m[1][1]+=(*data[i])[0];
		m[2][1]+=(*data[i])[0]*(*data[i])[0];
		m[3][1]+=(*data[i])[0]*(*data[i])[1];

		m[0][2]+=(*data[i])[2]*(*data[i])[1];
		m[1][2]+=(*data[i])[1];
		m[2][2]+=(*data[i])[0]*(*data[i])[1];
		m[3][2]+=(*data[i])[1]*(*data[i])[1];
	}

	for(int i=0;i<3;i++)
		cout <<m[0][i] << "  "<<m[1][i]<< "  "<<m[2][i]<< "  "<<m[3][i]<<endl;;

	double P1,P2,PA1,PA2,PB1,PB2,a,b,c,t;
	P1=m[0][0]*m[3][1]-m[0][1]*m[3][0];
	P2=m[0][0]*m[3][2]-m[0][2]*m[3][0];

	PA1=m[1][0]*m[3][1]-m[1][1]*m[3][0];
	PA2=m[1][0]*m[3][2]-m[1][2]*m[3][0];

	PB1=m[2][0]*m[3][1]-m[2][1]*m[3][0];
	PB2=m[2][0]*m[3][2]-m[2][2]*m[3][0];

        t=PA1*PB2-PA2*PB1;
        if(t==0)
        {
                cerr << ":: ERROR: Tilt calculation failed." << endl;
                exit(-1);
        }       

	a=(P1*PB2-P2*PB1)/(PA1*PB2-PA2*PB1);

        t=PB1;
        if(t==0)
        {
                cerr << ":: ERROR: Tilt calculation failed." << endl;
                exit(-1);
        }       

	b=(P1-PA1*a)/PB1;

        t=m[3][0];
        if(t==0)
        {
                cerr << ":: ERROR: Tilt calculation failed." << endl;
                exit(-1);
        }       

	c=(m[0][0]-m[1][0]*a-m[2][0]*b)/m[3][0];

	Cvector<double> ori(3),n(3),x(3),axis(3),e(3),olde(3);
	e[0]=b;e[1]=c;e[2]=-1;
	ori[0]=0,ori[1]=0,ori[2]=0;
	n[0]=0,n[1]=0,n[2]=1;
	x[0]=1,x[1]=0,x[2]=0;
	Cplane<double> pl(ori,e);
	Cplane<double> zero(ori,n);
	double sign=pl.n[1]<=0?1:-1;

        cout << endl << "Tilt plane is :" << endl;
	cout << "e[0]=" << e[0] << "    e[1]=" << e[1] << "    e[2]=" << e[2] << endl;

	cout << endl << "Determination from ";
 	cout << data.size() << " locations:" << endl;
        cout << "   TLTANG = ";
	cout<<reduce_angle(sign*angle(zero,pl),M_PI)/M_PI*180;
        cout << "       TLTAXIS = ";
	w=cross(pl.n,zero.n);
	sign=w[0]*w[1]<=0?-1:1; 
	double ang1 = angle(x,w);
	if(ang1>M_PI/2.0) ang1-=M_PI;
	ang1=fabs(ang1);
	cout<<reduce_angle(sign*ang1,M_PI)/M_PI*180<<endl;

// calculate new plane with the good points (5x)

	double oldrlimit,rlimit;
	rlimit = 3000.0;
	double rTLTANG,rTLTAXIS;
 
        int good[1000];

	for(int j=0,jtotal=0;j<50 && jtotal<1000;j++,jtotal++)
	{

	    	for(int i=0;i<4;i++ ) 
			for(int j=0;j<3;j++)
				m[i][j]=0.0;

		int icount = 0;
		for(int i=0;i<data.size();i++)
		{
			// only use points if distance to plane is < rlimit
			if(fabs(a+b*(*data[i])[0]+c*(*data[i])[1]-(*data[i])[2])<rlimit)
			{	m[0][0]+=(*data[i])[2];
				m[1][0]+=1;
				m[2][0]+=(*data[i])[0];
				m[3][0]+=(*data[i])[1];

				m[0][1]+=(*data[i])[2]*(*data[i])[0];
				m[1][1]+=(*data[i])[0];
				m[2][1]+=(*data[i])[0]*(*data[i])[0];
				m[3][1]+=(*data[i])[0]*(*data[i])[1];

				m[0][2]+=(*data[i])[2]*(*data[i])[1];
				m[1][2]+=(*data[i])[1];
				m[2][2]+=(*data[i])[0]*(*data[i])[1];
				m[3][2]+=(*data[i])[1]*(*data[i])[1];
				icount += 1;
                                good[i]=1;
			}
                        else
                        {
                                // dataset i is bad
                                good[i]=0;
                        }
		}

		Cplane<double> pl2(ori,e);

		if(icount < 16) {
			rlimit *= 1.2;
			rlimit += 100;
		} else {
			P1=m[0][0]*m[3][1]-m[0][1]*m[3][0];
			P2=m[0][0]*m[3][2]-m[0][2]*m[3][0];

			PA1=m[1][0]*m[3][1]-m[1][1]*m[3][0];
			PA2=m[1][0]*m[3][2]-m[1][2]*m[3][0];

			PB1=m[2][0]*m[3][1]-m[2][1]*m[3][0];
			PB2=m[2][0]*m[3][2]-m[2][2]*m[3][0];

			a=(P1*PB2-P2*PB1)/(PA1*PB2-PA2*PB1);
			b=(P1-PA1*a)/PB1;
			c=(m[0][0]-m[1][0]*a-m[2][0]*b)/m[3][0];

			e[0]=b;e[1]=c;e[2]=-1;
			sign=pl2.n[1]<=0?1:-1;
			
			j--;
		}

		if(icount > 24) {
			rlimit *= 0.9;
			rlimit -= 50;
			if(rlimit < 200.0) rlimit = 200.0;
		}

        	cout << endl << jtotal << ":  Tilt plane is :" << endl;
		cout << "e[0]=" << e[0] << "    e[1]=" << e[1] << "    e[2]=" << e[2] << endl;

		cout << endl << "New rlimit =  " << rlimit << endl;

		if(j==49 || jtotal==999)
		{
			cout << endl << " Differences of fitted plane are: " << endl << endl;
			for(int i=0;i<data.size();i++)
			{
				cout<< (*data[i])[0] << "     " << (*data[i])[1] << "     ";
				cout << a+b*(*data[i])[0]+c*(*data[i])[1]-(*data[i])[2] << endl;
			}
		}
 
		cout << endl << "Determination from " << icount << " of ";
 		cout << data.size() << " locations:" << endl;
                cout << "   TLTANG = ";
		rTLTANG = reduce_angle(sign* angle(zero,pl2),M_PI)/M_PI*180;
		cout<<rTLTANG;
                cout << "       TLTAXIS = ";
		w=cross(pl2.n,zero.n);
		sign=w[0]*w[1]<=0?-1:1; 
		double ang1 = angle(x,w);
		if(ang1>M_PI/2.0) ang1-=M_PI;
		ang1=fabs(ang1);
		rTLTAXIS = reduce_angle(sign*ang1,M_PI)/M_PI*180;
		cout<<rTLTAXIS<<endl;


                if(olde[0] == e[0] && olde[1] == e[1] && olde[2] == e[2] && oldrlimit == rlimit)
                {
                	jtotal += 100;
                }
                olde[0] = e[0];
                olde[1] = e[1];
                olde[2] = e[2];
                oldrlimit = rlimit;
	}

	ofstream resu( "2dx_tiltgeom.out", ios::out); //creates new outfile
        if (!resu)
	{
                cerr << "ERROR: Not able to create output file 2dx_tiltgeom.out ."<< endl;
		cerr  << "File already exists." << endl;;
                exit(-1);
	};
	resu << "set TLTANG = " << rTLTANG << endl;
	resu << "set TLTAXIS = "<< rTLTAXIS << endl;


        ofstream rout(name, ios::out); //creates new outfile
        if (!name){
                cerr << "ERROR: Not able to create output file ";
                cerr << name << ". File already exists \n";
                exit(-1);};

        rout << "#" << endl;
        rout << "\\rm -f TMP.txt" << endl;
        rout << "#" << endl;

	// write out good and bad ones
        int ig1 = 0;
	for(int i=1;i<tilenumber+1;i++)
	{
		for(int j=1;j<tilenumber+1;j++)
		{
                        rout << "echo \"#\"#" << endl;
                        rout << "echo \"###################################################################################################\"" << endl;
                        rout << "echo \"Working on position \"" << i << "," << j << endl;
                        rout << "echo \"###################################################################################################\"" << endl;
                        rout << "echo \"#\"#" << endl;
                        if(good[ig1]>0)
                        { 
                                rout << "# good: " << ig1 << " " << i << "," << j << " " << (*data[ig1])[0] <<  " " << (*data[ig1])[1] << " = " << (*data[ig1])[2] << endl;
                                rout << "\\rm -f CUT/${nonmaskimagename}.marked." << i << "." << j << ".ps.mrc" << endl;
                                rout << "\\cp -f CUT/${nonmaskimagename}." << i << "." << j << ".ps.mrc ";
                                rout << "CUT/${nonmaskimagename}.marked." << i << "." << j << ".ps.mrc" << endl;
                                rout << "#" << endl;
                                rout << "${bin_2dx}/2dx_maintain_defocus_table.exe << eot" << endl;
                                rout << "${defocus_pos_select_file}" << endl;
                                rout << i << "," << j << endl;
                                rout << "TMP.txt" << endl;
                                rout << "0" << endl;
                                rout << (*data[ig1])[2] << endl;
                                rout << "eot" << endl;
                                rout << "#" << endl;
                                rout << "echo \"# IMAGE: CUT/${nonmaskimagename}.marked." << i << "." << j << ".ps.mrc <CUT-PS-Marked_" << i << "," << j << ">\" >> LOGS/${scriptname}.results" << endl;
                                rout << "#" << endl;
                                rout << "\\mv -f TMP.txt ${defocus_pos_select_file}" << endl;
                                rout << "#" << endl;
                        }
                        else
                        {
                                rout << "# bad:  " << ig1 << " " << i << "," << j << " " << (*data[ig1])[0] << " " << (*data[ig1])[1] << " = " << (*data[ig1])[2] << endl;
                                rout << "\\rm -f CUT/${nonmaskimagename}.marked." << i << "." << j << ".ps.mrc" << endl;
                                rout << "${bin_2dx}/labelh.exe << eot" << endl;
                                rout << "CUT/${nonmaskimagename}." << i << "." << j << ".ps.mrc" << endl;
                                rout << "15" << endl;
                                rout << "CUT/${nonmaskimagename}.marked." << i << "." << j << ".ps.mrc" << endl;
                                rout << "eot" << endl;
                                rout << "#" << endl;
                                rout << "${bin_2dx}/2dx_maintain_defocus_table.exe << eot" << endl;
                                rout << "${defocus_pos_select_file}" << endl;
                                rout << i << "," << j << endl;
                                rout << "TMP.txt" << endl;
                                rout << "0" << endl;
                                rout << "0.0" << endl;
                                rout << "eot" << endl;
                                rout << "#" << endl;
                                rout << "echo \"# IMAGE: CUT/${nonmaskimagename}.marked." << i << "." << j << ".ps.mrc <CUT-PS-Marked_" << i << "," << j << ">\" >> LOGS/${scriptname}.results" << endl;
                                rout << "#" << endl;
                                rout << "\\mv -f TMP.txt ${defocus_pos_select_file}" << endl;
                                rout << "#" << endl;

                        }
                        ig1++;
                }
        }

	// memory deallocation
	for(int i=0;i<data.size();i++)
		delete data[i];
}


