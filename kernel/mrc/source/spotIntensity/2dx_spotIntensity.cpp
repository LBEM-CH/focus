#include <QTextStream>
#include <QMap>
#include <QMapIterator>
#include <QDebug>
#include <iostream>
#include <string>
#include <vector>
#include <math.h>
#include <mrcImage.h>
#include <confData.h>
using namespace std;

#define PI 3.145926

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

void getLattice(char *argv, double *l)
{
  vector<string> results;
  string t = argv, d = ",";
  split(t,d,results);
  for(unsigned int i =0;i<4 && i<results.size();i++)
    l[i] = atof(results.at(i).c_str());
}

double getSpotIntensity(float x, float y, float r1, float r2, mrcImage *image, QStringList &entry)
{
  
  double sum = 0.0, max = 0.0, background = 0.0;
  double value;
  
  int k = 0, b = 0;

  for(int j=-r1;j<=r1;j++)
    for(int i=-r1;i<=r1;i++)
    {
      if(i*i+j*j<r1*r1)
      {
        if(x+i>=0)
          value=image->intensity((int)round(x)+i,(int)round(y)+j+image->height()/2);
        else
          value=image->intensity(-((int)round(x)+i),-((int)round(y)+j)+image->height()/2);
       
        if(max<value) max = value;
        sum+=value;
        k++;
      }
    }

  for(int l=0;l<2.0*r2*PI;l++)
  {
    float i = r2*cos((float)l/(r2)), j = r2*sin((float)l/(r2));

    if(x+i>=0)
      value=image->intensity((int)round(x+i),(int)round(y+j)+image->height()/2);
    else
      value=image->intensity(-((int)round(x+i)),-((int)round(y+j))+image->height()/2);

    background+=value;
  }
   
  b = 2*r2*PI;

  background = background/b;//sqrt(background);
  //cout<<"Background: "<<sqrt(background)<<endl;
  //cout<<"Average: "<<sum/k<<endl;
  //cout<<(max)/(background)<<endl;
  entry<<QString::number(sum)<<QString::number(sum/k)<<QString::number(max)<<QString::number(background)<<QString::number(sum/k/(background));
  return sum;
  
}

bool recordLessThan(const QStringList &a, const QStringList &b)
{
  return a.first()<b.first();
}


double mag(float x, float y)
{
  return sqrt(x*x+y*y);
}

void run(int, char **argv)
{

  //int h = QString(argv[2]).section(',',0,0).toInt(), k=QString(argv[2]).section(',',1,1).toInt();
  //cout<<"Miller Index,"<<h<<","<<k<<endl;
  //cout<<"File,Index,Sum,Average,Maximum,Background,SNR"<<endl;
//  cout<<"Starting"<<endl;

  QStringList fileList;
  QFile file(argv[1]);
  if(!file.open(QIODevice::ReadOnly | QIODevice::Text)) {cout<<"Couldn't open file"<<endl; exit(-1);}
  QTextStream in(&file);
  while(!in.atEnd())
   fileList<<in.readLine(); 
  file.close();

  
  QMap<QString, QList<QStringList> > *entryList = new QMap<QString, QList<QStringList> >;
  QMap<QString,QStringList> headers;
  QStringList resolutions;

  float maxIndex = QString(argv[2]).toFloat();
  
  // = QString(argv[3]).toFloat();

  for(int h=0;h<=maxIndex;h++)
    for(int k=-maxIndex;k<=maxIndex;k++)
		{
	    if(h!=0 || (h==0 && k>=0))
			{

        float resolution=0;
				cout<<((k+maxIndex)+h*(2*maxIndex+1))/((maxIndex+1)*(2*maxIndex+1))*100.0<<"% complete"<<endl;
				cout<<"<<@progress: "<<((k+maxIndex)+h*(2*maxIndex+1))/((maxIndex+1)*(2*maxIndex+1))*100.0<<">>"<<endl;
				//QFile out("out.dat");
				//if(!out.open(QIODevice::ReadOnly | QIODevice::Text)) {cout<<"Couldn't open file"<<endl; return -1;}
				//headers<<"("+QString::number(h)+"/"+QString::number(k)+")";
				cout.flush();
				QList<QStringList> entries;
        int c = 0;
				foreach(QString line, fileList)
				{
					QStringList args = line.split(" ");
					if(args.size()>0)
					{
						confData data(args.last().trimmed());
						double lattice[4];
						float scale;
						getLattice(data.get("lattice","value").trimmed().toAscii().data(),lattice);
            float sideLength = data.get("imageSideLength","value").trimmed().toFloat();
            float magnification = data.get("magnification","value").trimmed().toFloat();
            float stepDigitizer = data.get("stepdigitizer","value").trimmed().toFloat();
            scale = sideLength*stepDigitizer*1e4/magnification;

						QString imageName = args.first().trimmed();
						mrcImage *image = new mrcImage(imageName.toAscii().data());
						if(image->isEmpty()) {cout<<"Invalid file name."<<endl;}

						float r = mag(h*lattice[0]+k*lattice[2],h*lattice[1]+k*lattice[3]);
            resolution+=scale/r;
//            if(!(h==0 && k==0)) cout<<scale<<" "<<resolution<<" "<<r<<" "<<resolution/(c+1)<<endl;

						QStringList entry;
						QRegExp reg("[a-zA-Z]{4,5}([0-9]*)\\.fft\\.mrc");
						reg.indexIn(QFileInfo(imageName).fileName());
						entry<<reg.cap(1);//.toStdString()<<",";//<<h<<","<<k<<",";
						getSpotIntensity(h*lattice[0]+k*lattice[2],h*lattice[1]+k*lattice[3],2,10,image,entry);
						entries<<entry;

						delete image;
						c++;
					}
				}
        
				resolution/=c;
        QStringList list;
        list<<"("+QString::number(h)+"/"+QString::number(k)+")"<<QString::number(resolution);
        headers.insert(QString::number(h)+"-"+QString::number(k),list);

				qStableSort(entries.begin(),entries.end(),recordLessThan);
				entryList->insert(QString::number(h)+"-"+QString::number(k),entries);

				QString hS = QString::number(h), kS = QString::number(k);

				QFile statsFile("HK-"+hS+"-"+kS+".csv");
				if(!statsFile.open(QIODevice::WriteOnly | QIODevice::Text)) {cout<<"Couldn't open file"<<endl; exit(-1);}
				QTextStream out(&statsFile);

				out<<"Miller Index,"<<hS<<","<<kS<<"\n";
				out<<"Resolution,"<<resolution<<"\n";
				out<<"File,Index,Sum,Average,Maximum,Background,SNR"<<endl;
				int index = 0;
				foreach(QStringList entry, entries)
				{
					out<<entry[0]<<","<<index<<",";
					for(int i=1;i<entry.size();i++)
					{
						out<<entry[i];
						if(i!=entry.size()-1) out<<",";
					}
					out<<'\n';
					index++;
				}
				statsFile.close();
			}
		}

	QStringList statisticsFiles; 
	statisticsFiles<<"Sum"<<"Average"<<"Maximum"<<"Background"<<"SNR";
	int k=0;
	foreach(QString stat, statisticsFiles)
	{
		QFile statsFile(stat+".csv");
		if(!statsFile.open(QIODevice::WriteOnly | QIODevice::Text)) {cout<<"Couldn't open file"<<endl; exit(-1);}
		QTextStream out(&statsFile);
		out<<"Statistics by Miller Index,"<<stat<<"\n\n";
    out<<",,";

    QMapIterator<QString, QStringList> hIt(headers);
    while(hIt.hasNext())
    {
      hIt.next();
      out<<hIt.value().first()<<",";
    }

		//foreach(QString header, headers)
		//	out<<header<<",";
		out<<"\n";

		out<<"File,Index,";
	   QMapIterator<QString, QStringList> hIt2(headers);
    while(hIt2.hasNext())
    {
      hIt2.next();
      out<<hIt2.value().last()<<",";
    }

	//foreach(QString resolution, resolutions)
	//		out<<resolution<<",";
		out<<"\n";

		for(int i=0;i<(*entryList)["0-0"].size();i++)
		{
			out<<(*entryList)["0-0"][i][0]<<","<<i<<",";
			QMapIterator<QString, QList<QStringList> > it(*entryList);
			while(it.hasNext())
			{
				it.next();
				QString series = it.key();
				QStringList entries = it.value()[i];
				out<<entries[k+1];
				if(it.hasNext()) out<<",";
			}
			out<<'\n';
		}
		statsFile.close();
		k++;
	}
}

int main(int argc, char **argv)
{
  if(argc<3) 
  {
    qDebug()<<"2dx_spotIntensity:\nUsed for determining resolution dependent information from series of images of identical samples.\n\nUsage:\n2dx_spotIntensity.exe ${filename} ${maxhklrange}\n\n${filename} should be a file describing location of file to be tested followed by a space, followed by its associated 2dx_image.cfg."; 
    qDebug()<<"${maxhklrange} describes the range (in miller indices) to be considered. ${maxhklrange} of 8 would result in all miller indices from -8 to 8 being considered for both lattice vectors.";
    return 0;
  }
  QApplication app(argc,argv);
  run(argc,argv);
  return 0;
}
