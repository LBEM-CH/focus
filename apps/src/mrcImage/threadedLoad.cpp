#include <threadedLoad.h>

float loadThread::fastMagnitude(float a, float b)
{
  return sqrt(a*a+b*b);
}

loadThread::loadThread(char *data, uchar *iData, quint32 width, quint32 height, float min, float max, int m, quint32 l, quint32 r, QImage::Format f, loadThread::loadType t, bool p, QObject *parent)
           :QThread(parent)
{
  rawData = data;
  imageData = iData;
  mode = m;
  leftI = l;
  rightI = r;
  showPhase = p;
  nx = width;
  ny = height;
  imageMin = min;
  imageMax = max;
  format = f;
  type = t;
}

void loadThread::run()
{
	if(type == loadThread::fft)
	{
		quint32 i,j,index;
		QColor color, oppositeColor;
		int colors[6];
		float theta, theta2, rotatePhase = 1.0;
		int value = 0;
		qint64 dataSize = nx*ny;

		for(quint32 k=leftI;k<rightI;k++)
		{
			i = k % (nx-1); j = k/(nx-1);
			index = (j*(nx) + i)*2;
			if(mode == 4) value = int(((fastMagnitude(((float*)rawData)[index],((float*)rawData)[index+1])-imageMin)/(imageMax-imageMin))*255.0);
			else if(mode == 3) value = int(((fastMagnitude(((short*)rawData)[index],((short*)rawData)[index+1])-imageMin)/(imageMax-imageMin))*255.0);
			if(value>255) value = 255;
			if(value<0) value = 0;

			if(showPhase)
			{
				rotatePhase = powf(-1,i+j);
				if(mode == 4) theta = (atan2(((float*)rawData)[index+1]*rotatePhase,((float*)rawData)[index]*rotatePhase)+2.0*PI)/(2.0*PI);
				else if(mode == 3) theta = (atan2(((short*)rawData)[index+1]*rotatePhase,((short*)rawData)[index]*rotatePhase)+2.0*PI)/(2.0*PI);
				theta = fmod(theta,1.0);

				if(theta<0.0) theta = 0.0;
				if(theta>1.0) theta = 1.0;

				color.setHsvF(theta, 1.0, value/255.0);
				theta2 = 1.0 - theta;
				if(theta2 > 1.0) theta2 = 1.0;
				if(theta2 < 0.0) theta2 = 0.0;
				oppositeColor.setHsvF(theta2, 1.0, value/255.0);
				color = color.toRgb();
				oppositeColor = oppositeColor.toRgb();
				colors[0] = color.red();
				colors[1] = color.blue();
				colors[2] = color.green();
				colors[3] = oppositeColor.red();
				colors[4] = oppositeColor.blue();
				colors[5] = oppositeColor.green();
			}
			else
				for(int c=0;c<6;c++)
					colors[c] = value;

			if(format ==  QImage::Format_RGB32)
			{
				if(j!=0 && i!=0) ((unsigned int*)imageData)[((ny-j)*(2*(nx-1))+(2*(nx-1)-1-i)-(nx-1))] = qRgb(colors[3],colors[4],colors[5]);
				((unsigned int*)imageData)[((j)*(2*(nx-1))+i+(nx-1)-1)] = qRgb(colors[0],colors[1],colors[2]);

				if(j==0) ((unsigned int*)imageData)[i]=0;
				if(i==(nx-1)-1) ((unsigned int*)imageData)[((j)*(2*(nx-1))+(2*(nx-1)-1))]=0;
			}
			else if(format == QImage::Format_Indexed8)
			{
                                /* CHEN:
				imageData[((j)*(2*(nx-1))+i+(nx-1)-1)] = value;
				if(j!=0) imageData[((ny-j)*(2*(nx-1))+(2*(nx-1)-1-i)-(nx-1))] = value;
				else imageData[i]=0;
				if(i==(nx-1)-1) imageData[((j)*(2*(nx-1))+(2*(nx-1)-1))]=0;
                                */
				imageData[((j)*(2*(nx-1))+i+(nx-1)-1)] = value;
				if(j!=0) imageData[((ny-j)*(2*(nx-1))+(2*(nx-1)-1-i)-(nx-1))] = value;
				else imageData[i]=0;
				if(i==(nx-1)-1) imageData[((j)*(2*(nx-1))+(2*(nx-1)-1))]=0;
			} 
			else return;
		}   
	}
  else if(type == loadThread::real)
	{
		int width;
                int value = 0;
		if(format == QImage::Format_Indexed8) width = 1;
		else if(format == QImage::Format_RGB32) width = 4;
		else return;

		for(quint32 i=leftI;i<rightI;i++)
		{
			if (mode == 0)
			{
				value = int((((uchar*)rawData)[i/width]-imageMin)/(imageMax-imageMin)*255.0);
			}
			else if(mode == 1)
			{
				value = int((((unsigned short*)rawData)[i/width]-imageMin)/(imageMax-imageMin)*255.0);
			}
			else if(mode == 2)
			{
				value =int((((float*)rawData)[i/width]-imageMin)/(imageMax-imageMin)*255.0);
			}

			if(format == QImage::Format_RGB32)
			{
				if(value>255) value = 255; if(value<0) value = 0;
				((int*)imageData)[i/width] = qRgb(value,value,value);
			}
			else
			{
				imageData[i] = uchar(value);
			}
		}
	}
}

