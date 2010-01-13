#include <largeMRC.h>


void largeMRC::initialize()
{
  mode = header->mode();
  if(mode == 0) cellSize = 1;
  else if(mode == 1) cellSize = 2;
  else if(mode == 2) cellSize = 4;
  else if(mode == 3) cellSize = 4;
  else if(mode == 4) cellSize = 8;

  isComplex = (mode == 3 | mode == 4);

  nx = header->nx();
  ny = header->ny();
  nz = header->nz(); if(nz==0) nz = 1;
  data = new char[cellSize*nx*ny*nz];

  min = header->min();
  max = header->max();
 
  if(isComplex) max*=0.001;

	w = 512;
  
  //QPixmapCache::clear();
  int pixmapSize = 500;
  if(QPixmapCache::cacheLimit()!=pixmapSize*1024) QPixmapCache::setCacheLimit(pixmapSize*1024);
}

largeMRC::largeMRC(const QString &fName, QObject *)
{
  fileName = fName;
  data = NULL;

  header = new mrcHeader(fileName); 
  fileValid = header->isValid();
  if(fileValid) initialize();
}

largeMRC::~largeMRC()
{
  if(data!=NULL) delete data;
  if(header!=NULL) delete header;
}

bool largeMRC::load(quint32 index)
{
  int height = w;
  if((int)index*(int)w>(int)ny) return false;
  if((int)index*(int)w>(int)ny-(int)w) height = (int)ny-(int)index*(int)w;
  QFile imageFile(fileName);
  if(!imageFile.open(QIODevice::ReadOnly)) return false;
  imageFile.seek(nx*cellSize*w*index+1024);
  imageFile.read(&(data[nx*cellSize*w*index]),nx*cellSize*height);
	imageFile.close();
  return true;
}

float largeMRC::fastMagnitude(float a, float b)
{
  return sqrt(a*a+b*b);
}

float largeMRC::amp(quint32 x, quint32 y)
{
  quint32 index = (x+nx*y);
	if(mode == 0) return float(((unsigned char*)data)[index]);
	if(mode == 1) return float(((short*)data)[index]);
	if(mode == 2) return ((float*)data)[index];
	if(mode == 3) return fastMagnitude(((short*)data)[2*index],((short*)data)[2*index+1]);
	if(mode == 4) return fastMagnitude(((float*)data)[2*index],((float*)data)[2*index+1]);
  return 0.0;
}

QPixmap largeMRC::pixmap(int x, int y)
{
  bool rightHalf = (x>=0);
  if(x<0) x*=-1;
  if(y<0) y*=-1;
  int width = pixmapWidth(x,y), height=pixmapHeight(x,y);
  int xIndex = x/w, yIndex = y/w;
  
  QPixmap p; 
 
  if(y>int(ny)) return p;

  QString key; 
  if(!rightHalf)
		key = fileName + " - I - " + QString::number(xIndex) + ',' + QString::number(yIndex);
  else
		key = fileName + " - " + QString::number(xIndex) + ',' + QString::number(yIndex);

	if(!QPixmapCache::find(key,p)) 
	{
	  if(!loaded.contains(yIndex)) 
    {
      mutex.lock(); 
			//mutex.tryLock(3000);
      load(yIndex);
      loaded<<yIndex;
      mutex.unlock();
    }
  
  	uchar *temp = new uchar[(width)*height*4];

    float v;
		for(int j=0;j<height;j++)
			for(int i=0;i<width;i++)
			{
        v=(amp(i+x,j+y)-min)/(max-min)*255.0; 
        if(v<0) v= 0.0; if(v>255.0) v = 255.0;
				((unsigned int*)temp) [i+j*(width)] = qRgb((int)v,(int)v,(int)v);
			}

		QImage *img = new QImage(temp,width,height,QImage::Format_RGB32);
    if(!img->isNull())
    {
/*		  img->setNumColors(256);
		  int i;
		  for(i=0;i<256;i++)
	  		img->setColor(i,QColor(i,i,i).rgb());
*/
			if(rightHalf)
				p = QPixmap::fromImage(*img);
			else 
				p = QPixmap::fromImage(img->mirrored(true,true));
    }
	  else
      qDebug()<<"Null image!";
		delete img;
		delete temp;
    //mutex.tryLock(3000);
    mutex.lock();
		QPixmapCache::insert(key,p); 
    mutex.unlock();
	}
	return p;
}

int largeMRC::pixmapWidth(int x, int y)
{
  int width = w;
  if(x<0) x*=-1;
	if(x>=(int)nx) return -1;

	if((x)>int(nx)-int(w)) width = (nx-x);
  return width;
}


int largeMRC::pixmapHeight(int x, int y)
{
  int height = w;
  if(y<0) y*=-1;
  if(y>=(int)ny) return -1;
	if((y)>int(ny)-int(w)) height = (ny-y);
  return height;
}

quint32 largeMRC::width()
{
  if(isComplex) return (nx)*2;
  return nx;
}

quint32 largeMRC::height()
{
  return ny;
}

quint32 largeMRC::slice()
{
  return w;
}

bool largeMRC::isFFT()
{
  return isComplex;
}

