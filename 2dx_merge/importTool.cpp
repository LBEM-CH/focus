#include <importTool.h>

importTool::importTool(confData *conf, const QStringList &importList, QWidget *parent)
					:QDialog(parent)
{
  setAttribute(Qt::WA_DeleteOnClose);
  setModal(true);
  ui.setupUi(this); 
  
  data = conf;

  fileList=importList;
  foreach(QString file, fileList)
    nameList<<QFileInfo(file).baseName();
  
  pattern.setCaseSensitivity(Qt::CaseInsensitive);
  
  parsedHeaderLabels<<"Name"<<"Angle"<<"Number"<<"Sub-Image";
  
  QStringList fileViewHeaders;
  fileViewHeaders<<"Filename";
  ui.importView->setColumnCount(1);  
  ui.importView->setRowCount(nameList.size()+1);  
  ui.importView->setHorizontalHeaderLabels(fileViewHeaders);
  QTableWidgetItem *item;
  ui.importView->setItem(0,0,new QTableWidgetItem);
  for(int i=1;i<=nameList.size();i++)
  {
    item=new QTableWidgetItem(QFileInfo(fileList[i-1]).fileName());
    ui.importView->setItem(i,0,item);    
  }

  ui.importView->resizeRowsToContents();
  
  ui.parsedView->setHorizontalHeaderLabels(parsedHeaderLabels);
  ui.parsedView->setColumnCount(parsedHeaderLabels.size());
  ui.parsedView->setRowCount(nameList.size()+1);
  ui.parsedView->resizeRowsToContents();
  ui.parsedView->resizeColumnsToContents();
  for(int i=0;i<4;i++)
  {
    item=new QTableWidgetItem;
    item->setBackground( QColor(227,233,244));
    ui.parsedView->setItem(0,i,item);
  }
  connect(ui.parsedView,SIGNAL(cellChanged(int,int)),this,SLOT(updateParsedViewRequired(int,int)));
  ui.patternInput->setEditable(true);
  QString toolTipText = "Enter regular expressions to be used in translating file names.<br>";
  toolTipText += "Each column of the below view is drawn from ordered parentheses occuring in the expression.<br>";
  toolTipText += "For instance:<br> ^(\\w{4})(\\d{2})(\\d{6})(\\d{2})?$<br>";
  toolTipText += "Breaks the file name: PROT00123456700 into (PROT)(00)(123456)(00)<br><br>";
  toolTipText += "For more information on RegExps try: http://www.regular-expressions.info/";
  ui.patternInput->setToolTip(toolTipText);
  connect(ui.patternInput,SIGNAL(editTextChanged(const QString &)),this,SLOT(setRegExp(const QString &)));
  connect(ui.addRegExpButton,SIGNAL(released()),this,SLOT(addPattern()));
  connect(ui.removRegExpButton,SIGNAL(released()),this,SLOT(removePattern()));
  
  if(!loadPatternList())
  {
    if(loadPatternList(data->getDir("config") + "/2dx_merge/" + "importPatterns.inf"))
      savePatternList();
    else
      qDebug()<<"Failed to save "<<data->getDir("config") + "/2dx_merge/" + "importPatterns.inf";
  }
  
  updateParsedView("^$");
  
  show();
}

void importTool::setRegExp(const QString &regExp)
{
  updateParsedView(regExp);
}

void importTool::updateParsedView()
{
  updateParsedView(QString());
}

void importTool::updateParsedView(const QString &regExp)
{
  disconnect(ui.parsedView,SIGNAL(cellChanged(int,int)),this,SLOT(updateParsedViewRequired(int,int)));
  pattern.setPattern(regExp);
  if(regExp.isEmpty() || pattern.errorString().isEmpty() || pattern.errorString() == "no error occurred") 
  {
    ui.label->hide();
    ui.patternMessage->hide();
  }
  else 
  {
    ui.label->show();
    ui.patternMessage->show();
    ui.patternMessage->setText(pattern.errorString()); 
  }
  
//  ui.parsedView->clear();
//  ui.parsedView->setHorizontalHeaderLabels(parsedHeaderLabels);
//  ui.parsedView->setColumnCount(parsedHeaderLabels.size());
//  ui.parsedView->setRowCount(nameList.size());
  int j=1;
  int bestPattern, max, index;
  foreach(QString name, nameList)
  {
    max = 0; bestPattern = 0;
    int current = 0;
    bool matchedCurrent = false;
    for(int p=0;p<ui.patternInput->count();p++)
    {
      QString pI = ui.patternInput->itemText(p);
      pattern.setPattern(pI);
      index = pattern.indexIn(name);      
      //qDebug()<<"Index: "<<index;
      if(index!=-1 && pattern.numCaptures()>max) {max = pattern.numCaptures(); bestPattern=p;}
    }
    
    if(!regExp.isEmpty())
    {
      pattern.setPattern(regExp);
      if(pattern.indexIn(name)==-1 || pattern.numCaptures()<max)
      {
        pattern.setPattern(ui.patternInput->itemText(bestPattern));
        pattern.indexIn(name);
      }
      else
        matchedCurrent = true;
    }

   // qDebug()<<pattern.pattern()<<" "<<pattern.numCaptures();

    for(int i=0;i<4;i++)
    {
      QTableWidgetItem *item=ui.parsedView->item(j,i);
      if(item == NULL) item = new QTableWidgetItem;
      if(ui.parsedView->item(0,i)->text().trimmed().isEmpty())
        {
          if(matchedCurrent) current = 2;
          else if(!pattern.cap(i+1).trimmed().isEmpty()) current = 1;

          if(pattern.cap(i+1).trimmed().isEmpty())
          {
            item->setText(QString());
            current = 0;
          }
          else if(i==0)
            item->setText(QString("%1").arg(pattern.cap(i+1),-4,QChar('0')));
          else if(i==2)
            item->setText(QString("%1").arg(pattern.cap(i+1).toInt(),6,10,QChar('0')));
          else if(i==1 || i ==3)
            item->setText(QString("%1").arg(pattern.cap(i+1).toInt(),2,10,QChar('0')));
          else
            item->setText(pattern.cap(i+1));
        }
      //lse if(i!=2)
       // item->setText(ui.parsedView->item(0,i)->text());
      //else
      // item->setText(QString("%1").arg(ui.parsedView->item(0,i)->text().toInt()+j,6,10,QChar('0')));
      //if(pattern.numCaptures()>i)
        item->setData(33,current);
      ui.parsedView->setItem(j,i,item);
    }

    j++;                            
  }
  connect(ui.parsedView,SIGNAL(cellChanged(int,int)),this,SLOT(updateParsedViewRequired(int,int)));

  updateParsedViewRequired();
}

void importTool::updateParsedViewRequired()
{
  updateParsedViewRequired(-1,-1);
}

void importTool::updateParsedViewRequired(int m, int n)
{

  if(m==0 && ui.parsedView->item(m,n)->text().trimmed().isEmpty())
  {
    updateParsedView();
    return;
  }

  disconnect(ui.parsedView,SIGNAL(cellChanged(int,int)),this,SLOT(updateParsedViewRequired(int,int)));

  QTableWidgetItem *item;

  for(int j=1;j<ui.parsedView->rowCount();j++)
    for(int i=0;i<ui.parsedView->columnCount();i++)
      if(ui.parsedView->item(j,i)!=NULL)
      {
        QString patternText = ui.parsedView->item(0,i)->text();
        item = ui.parsedView->item(j,i);
        QColor textColor = Qt::white;

        if(j==m && i==n)
          item->setData(33,1);

        if(patternText.trimmed().isEmpty())
        {
          if(item->text().trimmed().isEmpty())
            textColor = (Qt::red);
          else if(item->data(33).toInt() == 2)
            textColor = (Qt::green);
          else if(item->data(33).toInt() == 1)
            textColor = (Qt::white);
          else
            textColor = Qt::red;
        }
        //else
            //textColor = Qt::white;

        if((!patternText.isEmpty() || item->text().isEmpty()))
        {
          if(i==0)
            item->setText(QString("%1").arg(patternText,-4,QChar('0')));
          else if(i==2)
          {
           //CHEN>  if(item->text().isEmpty())
           //CHEN>    item->setText(QString("%1").arg(patternText.toInt()+j,6,10,QChar('0')));
           //CHEN>  else
           //CHEN>    item->setText(QString("%1").arg(patternText.toInt()+item->text().toInt(),6,10,QChar('0')));
            item->setText(QString("%1").arg(patternText.toInt()+j-1,6,10,QChar('0')));
          }
          else if(i==1 || i ==3)
            item->setText(QString("%1").arg(patternText.toInt(),2,10,QChar('0')));
          else
            item->setText(patternText);
        }
        item->setBackground(textColor);

      }
  connect(ui.parsedView,SIGNAL(cellChanged(int,int)),this,SLOT(updateParsedViewRequired(int,int)));
}

void importTool::addPattern()
{
  ui.patternInput->insertItem(ui.patternInput->count(),ui.patternInput->lineEdit()->text());  
  ui.patternInput->setCurrentIndex(ui.patternInput->count()-1);
  updateParsedView();  
  savePatternList();
}

void importTool::removePattern()
{
  ui.patternInput->removeItem(ui.patternInput->currentIndex());
  updateParsedView();
  savePatternList();  
}

bool importTool::savePatternList()
{
  return savePatternList(data->getDir("home_2dx") + "/2dx_merge/" + "importPatterns.inf"); 
}

bool importTool::savePatternList(const QString &path)
{
  QFile file(path);
  if(!file.open(QIODevice::WriteOnly | QIODevice::Text)) return false;
  foreach(QString line, patternFileHeader)
    file.write((line + '\n').toAscii());
  file.write(QString('\n').toAscii());
  for(int i=0;i<ui.patternInput->count();i++)
    file.write((ui.patternInput->itemText(i) + '\n').toAscii());
  file.close();
  return true;
}

bool importTool::loadPatternList()
{
  return loadPatternList(data->getDir("home_2dx") + "/2dx_merge/" + "importPatterns.inf"); 
}

bool importTool::loadPatternList(const QString &path)
{
  QFile file(path);
  if(!file.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
  QTextStream stream(&file);
  QString line;

  while(!file.atEnd())
  {
    line = file.readLine().simplified();
    if(line.contains(QRegExp("^\\s*#"))) patternFileHeader<<line;
    else if(!line.isEmpty())
      ui.patternInput->addItem(line);
  }
  file.close();
  return true;
}

void importTool::show()
{
  QDialog::show();
  ui.importView->setColumnWidth(0,ui.importView->maximumViewportSize().width());
  float scaleFactor = 0.0;
  for(int i=0;i<ui.parsedView->columnCount();i++)
    scaleFactor+=(float)ui.parsedView->columnWidth(i);
  scaleFactor=(float)ui.parsedView->maximumViewportSize().width()/scaleFactor;
  for(int i=0;i<ui.parsedView->columnCount();i++)
    ui.parsedView->setColumnWidth(i,(int)(ui.parsedView->columnWidth(i)*scaleFactor));
}

void importTool::exportParsedList()
{
  imageList.clear();
  int j=1;
  foreach(QString file, fileList)
  {
    QHash<QString, QString> parsedFile;
    QString proteinCode, tiltAngle, frameNum, subNum;

    proteinCode = QString("%1").arg(ui.parsedView->item(j,0)->text(),-4,QChar('0'));
    if(proteinCode.trimmed().isEmpty()) proteinCode = "PROT";
    tiltAngle = QString("%1").arg(ui.parsedView->item(j,1)->text().toInt(),2,10,QChar('0'));
    frameNum = QString("%1").arg(ui.parsedView->item(j,2)->text().toInt(),6,10,QChar('0'));
    subNum = QString("%1").arg(ui.parsedView->item(j,3)->text().toInt(),2,10,QChar('0'));

    qDebug()<<proteinCode<<" "<<tiltAngle<<" "<<frameNum<<" "<<subNum;

    parsedFile.insert("protein_code",proteinCode);
    parsedFile.insert("tilt_angle",tiltAngle);
    parsedFile.insert("frame_number",frameNum);
    parsedFile.insert("sub_image_number",subNum);
    imageList.insert(file,parsedFile);
    j++;
  }
}

void importTool::accept()
{
  exportParsedList();
  emit acceptedImages(imageList);
  QDialog::accept();
  done(1);
}
