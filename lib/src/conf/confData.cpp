/*
 *  confData.cpp
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "confData.h"
#include <QDebug>
#include <iostream>
using namespace std;

void confData::init(const QString &fileName, confData *parentData)
{
  parentConf = parentData;
  dataFilename = fileName;
  valueSearch<<"LABEL"<<"LEGEND"<<"EXAMPLE"<<"FORMER"<<"HELP"<<"TYPE"<<"RELATION"<<"LOCKED"<<"CONCERNS"<<"USERLEVEL"<<"INHERITABLE_UPON_INIT"<<"SYNC_WITH_UPPER_LEVEL"<<"ISWRONG";
  userSetProperties<<"LOCKED"<<"SYNC_WITH_UPPER_LEVEL"<<"ISWRONG";
  //globalSetPropeties = valueSearch - userSetProperties
  for(int i=0; i<valueSearch.size(); ++i)
  {

	  if(!(userSetProperties.contains(valueSearch[i])))
	  globalSetProperties << valueSearch[i];
  }

  
  empty = false;
  OS_X_APP_PATH = "Contents/MacOS/";
  if(!parseDataFile())
  {
    empty = true;
    cout<<"Datafile read error: "<<fileName.toStdString()<<endl;
  }
  autoSave = false;
  setModified(false);
}

confData::confData(QString filename, confData *parentData, QObject *parent)
                  :QObject(parent)
{
  init(filename, parentData);
//  sync(QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg");
}

confData::confData(QString filename, confData *parentData, QString link, QObject *parent)
                  :QObject(parent)
{
  linkName = link;
  init(filename, parentData);
}
		  
confData::confData(const confData &data)
                  :QObject(data.parent())
{
  parentConf = data.parentConf;
  userData = data.userData;
  dataFilename = data.dataFilename;
  header = data.header;
  sections = data.sections;
  lookup = data.lookup;
  directories = data.directories;
  applications = data.applications;
  icons = data.icons;
  dataFilename = data.dataFilename;
  valueSearch = data.valueSearch;
  empty = data.empty;
  modified = data.modified;
  autoSave = data.autoSave;
 // sync(QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg");
}

confData::confData(const QString &source, const QString &reference)
{
  init(reference);
  confData local(source);
  loadConf(&local);
  setSaveName(source);
  //sync(QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg");
}

confData::confData(const QString &source, const QString &reference, const QString &link)
{
  linkName = link;	
  confData(source, reference);
}
confData::confData(QObject *parent)
         :QObject(parent)
{
  empty = false;
  OS_X_APP_PATH = "Contents/MacOS/";
  autoSave = false;
  setModified(false);
//  sync(QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg");
}

confData::~confData()
{
  clear();
}

void confData::setParentConf(confData *parent)
{
  parentConf = parent;
}

void confData::setUserConf(confData *userConf)
{
  userData = userConf;
}

void confData::setSymLink(const QString fileName, const QString linkName)
{
  QFile data(dataFilename);
  if(!data.open(QIODevice::WriteOnly | QIODevice::Text)) return;
  //dele the file that lies where link should be
  if(QFileInfo(linkName).exists())
    QFile(linkName).remove();
  data.link(fileName, linkName);
}


bool confData::isEmpty()
{
  return empty;
}

bool confData::hasParent()
{
  if(parentConf == NULL) return false;
  else return true;
}

void confData::clear()
{
}

void confData::loadDefaults()
{
  if(get("imagename","value").trimmed() == "ImageName")
  {
    QDir dir(getDir("working"));
    QStringList images = dir.entryList(QStringList() << "*.mrc", QDir::Files, QDir::Size);
    if(!images.empty())
    {
      images[0].chop(4);
      set("imagename",images[0]);
    }
    else
    {
      images = dir.entryList(QStringList() << "*.tif" << "*.tiff", QDir::Files, QDir::Size);
      if(!images.empty())
      {
        images[0].truncate(images[0].lastIndexOf('.'));
        set("imagename",images[0]);
      }
    }
  }
  emit loading();
}

QString confData::readLine(QFile &data)
{
  QString lineData = data.readLine();
  return lineData.trimmed();
}

void confData::verifyElement(confSection *&section, confElement *&e)
{
  if(section==NULL)
  {
    section = new confSection("  ",this);
    sections<<section;
  }

  if(e==NULL) e = new confElement(this);
  else return;
}

confSection* confData::getSection(const QString title)
{
  for (int i = 0; i < sections.size(); ++i) 
  {
    if (sections.at(i)->title() == title)
      return sections.at(i);
  }
  confSection* section = new confSection(title);
  //add the new section
  sections<<section;
  return section;
}


confElement* confData::getElement(confElement* element)
{
  if(element==NULL) 
    return new confElement(this);
  else
    return element;
}

confElement* confData::protectUserSetProperies(confElement* source, confElement* target)
{
   for(int i=0;i<userSetProperties.size();++i)
   { 
     if(source->get(userSetProperties[i]) != "")
      {
        target->set(userSetProperties[i].simplified(),source->get(userSetProperties[i]));
      }
   }
   return target;
}

QString &confData::parseVariables(QString &line)
{
  confData *dataSource = this;
  if(parentConf!=NULL) dataSource = parentConf;
  line.replace("${appDir_2dx}",dataSource->getDir("appDir"));
  line.replace("${scripts-standard_2dx}",dataSource->getDir("templateDir"));
  line.replace("${scripts-custom_2dx}",dataSource->getDir("customDir"));
  line.replace("${proc_2dx}",dataSource->getDir("procDir"));
  line.replace("${app_2dx_image}",dataSource->getApp("2dx_image"));
  line.replace("${app_2dx_merge}",dataSource->getApp("2dx_merge"));
  return line;
}

bool confData::saveInUpperLevel(QString variable, QString value)
{
    if(parentConf==NULL)
    {
        QString parentConfPath = QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg";
        if(QFileInfo(parentConfPath).exists())
        {
            parentConf = new confData(parentConfPath);
        }
        else
        {
            return false;
        }
    }
    //qDebug() << "syncing [" << variable << "=" << value << " to " << parentConf->getDataFilename();
    if(parentConf->set(variable,value)>0)
        return true;
    return false;
}

QString confData::printLookup()
{
  QString elementString;
  QHashIterator<QString, confElement *> i(lookup);
  while (i.hasNext()) {
       i.next();
       QString props =  i.value()->toString();
       elementString.append(i.key()); 
       elementString.append(props); 
  }
  return elementString;

}

bool confData::parseDataFile()
{
  QFile data(dataFilename);
  if(!data.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
  //data.setTextModeEnabled(true);
  //create symbolic link if lineName is set
  if(!linkName.isEmpty())
  {
    //qDebug()<< "creating the symlink to " << data.fileName() << " with the name " << linkName;
    data.link(data.fileName(), linkName);
  }
  QString lineData;
  confSection *section=NULL;
  confElement *element=NULL;

  bool headerRead=false;
  QString headerLine;
  qint64 pos = -1;
  while(!data.atEnd() && pos!=data.pos() && lineData.toLower()!="$end_local_vars" && lineData.toLower()!="$end_vars")
  {
    bool inValueSearch = false;
    pos = data.pos();
    headerLine = data.readLine().trimmed();
    lineData = headerLine;
    lineData.remove('#');
    lineData = lineData.trimmed();

    if(lineData.startsWith("===") && !headerRead) headerRead=true;
	
    if(lineData.toLower().startsWith("section:"))
    {
      if(!headerRead) headerRead=true;
        lineData.remove(0,8);
      section=new confSection(lineData.simplified(),this);
      sections << section;
    }

    if(lineData.toLower().startsWith("manual:"))
    {
      lineData.remove(0,7);
      parseVariables(lineData);
      manualData<<lineData.trimmed();
    }

    if(!headerRead)
      header<<headerLine;
	
    for(int i=0;i<valueSearch.size();i++)
      if(lineData.startsWith(valueSearch[i] + ':'))
      {
        verifyElement(section,element);
        lineData.remove(0,valueSearch[i].size()+1);
//        if(valueSearch[i] == "SYNC_WITH_UPPER_LEVEL")
//        	if(lineData.trimmed().toLower() == "no")
//        		qDebug() << "SYNC_WITH_UPPER_LEVEL: no";
        element->set(valueSearch[i].simplified(),lineData);
        inValueSearch = true;
      }

    if(!inValueSearch && !headerLine.toLower().contains(QRegExp("section\\s*:")) && headerLine.contains(QRegExp("^\\s*#\\s*\\w*\\s*:")))
    {
      headerLine.remove("#");
      QStringList cell = headerLine.split(':');
      if(cell.first().trimmed().toLower() == "global")
      {
        cell.first() = "display";
      }
      properties.insert(cell.first().trimmed().toLower(),cell.last().trimmed());
    }

    if(lineData.toLower().startsWith("set "))
    {
      verifyElement(section,element);
      int k = lineData.indexOf('=');
      if(k>0)
      {
        QStringList val = lineData.split('=');

        val[0].remove(0,4);
        val[1].remove('"');

        val[0]=val[0].simplified();
        val[1]=val[1].simplified();
		
        element->set("valueLabel",val[0]);
        element->set("value",val[1]);
		
        *section << element;
        lookup.insert(element->get("valuelabel").toLower(),element);
        section->addConcern(element->get("concerns"));
        element = NULL;
      }
    }
  }

  QRegExp sep("^\\s*source\\s+\\$\\{proc_2dx\\}\\/(\\S+)");
  while(!data.atEnd())
  {
    if(sep.indexIn(data.readLine()) != -1)
      subScript<<sep.cap(1);
  }

  data.close();
  return true;
}

void confData::updateConf(const QString &confFileName)
{
  //qDebug()<<"updating configuration";
  QFile data(confFileName);
  if(!data.open(QIODevice::ReadOnly | QIODevice::Text)) return;
  
  QString lineData;
  QList<confSection*> newSections;
  confSection *section=NULL;
  confElement *element=NULL;

  bool headerRead=false;
  QString headerLine;
  qint64 pos = -1;
  while(!data.atEnd() && pos!=data.pos() && lineData.toLower()!="$end_local_vars" && lineData.toLower()!="$end_vars")
  {
    bool inValueSearch = false;
    pos = data.pos();
    headerLine = data.readLine().trimmed();
    lineData = headerLine;
    lineData.remove('#');
    lineData = lineData.trimmed();

    if(lineData.startsWith("===") && !headerRead) headerRead=true;

    if(lineData.toLower().startsWith("section:"))
    {
      if(!headerRead) headerRead=true;
      lineData.remove(0,8);
      section=new confSection(lineData.simplified(),this);
      newSections << section;
    }
	
    for(int i=0;i<globalSetProperties.size();i++)
    {
      if(lineData.startsWith(globalSetProperties[i] + ':'))
      {
        element = getElement(element);
        lineData.remove(0,globalSetProperties[i].size()+1);
        element->set(globalSetProperties[i].simplified(),lineData);
        inValueSearch = true;
      }
    }

    if(!inValueSearch && !headerLine.toLower().contains(QRegExp("section\\s*:")) && headerLine.contains(QRegExp("^\\s*#\\s*\\w*\\s*:")))
    {
      headerLine.remove("#");
      QStringList cell = headerLine.split(':');
      if(cell.first().trimmed().toLower() == "global")
      {
        cell.first() = "display";
      }
      properties.insert(cell.first().trimmed().toLower(),cell.last().trimmed());
    }

    if(lineData.toLower().startsWith("set "))
    {
       element = getElement(element);
       int k = lineData.indexOf('=');
       if(k>0)
       {
         QStringList val = lineData.split('=');

         val[0].remove(0,4);
         val[1].remove('"');

         val[0]=val[0].simplified();
         val[1]=val[1].simplified();
   
         element->set("valueLabel",val[0]);
         QHash<QString, confElement *>::iterator it = lookup.find(val[0].toLower());
         QString value;
         //if the element does not exist yet 
         if(it == lookup.end())
           value = val[1];
         else
         {
           value = it.value()->get("value");
           element = protectUserSetProperies(it.value(),element);
         }

         element->set("value",value);
         if(section!=NULL)
           *section << element;
         lookup.insert(element->get("valuelabel").toLower(),element);
         section->addConcern(element->get("concerns"));
         element = NULL;
       }
    }
 }

  data.close();
  sections = newSections;
  //qDebug() << "=== updating configuration done";
}

unsigned int confData::size()
{
  return sections.size();
}

QList<confSection *> confData::getSections()
{
  return sections;
}

void confData::insertSections(QList<confSection *> sectionList)
{
  sections<<sectionList;
}

QHash<QString,confElement *> confData::getLookupTable()
{
  return lookup;
}

confSection* confData::operator[](unsigned int i)
{
  return sections[i];
}

void confData::load()
{
  clear();
  parseDataFile();
  setModified(false);
  emit loading();
}

void confData::reload()
{
  confData local(dataFilename);
  loadConf(&local);
  setModified(false);
}

void confData::loadConf(confData *conf)
{
  for(unsigned int i=0;i<conf->size();i++)
  {
    for(unsigned int j=0;j<(*conf)[i]->size();j++)
    {
      confElement *element = (*(*conf)[i])[j];
      QString valueLabel = element->get("valueLabel");
      confElement *data = get(valueLabel);
      //copying all the values in valueSearch
      if(data!=NULL)
      {
      	for(int k=0;k<valueSearch.size();k++)
          data->set(valueSearch[k], element->get(valueSearch[k]));
        if(!element->get("locked").isEmpty()) data->set("locked",element->get("locked").simplified());
        data->set("value", element->get("value"));
      }
    }
  }
  emit loading();
}


void confData::loadDefaultConf(confData *conf, const QStringList &defaults)
{
  foreach(QString d, defaults)
  {
    confElement *element = conf->get(d);
    if(element!=NULL)
    {
      QString valueLabel = element->get("valueLabel");
      confElement *data = get(valueLabel);
      if(data!=NULL)
      {
        if(!element->get("locked").isEmpty()) data->set("locked",element->get("locked"));
        data->set("value", element->get("value"));
      }
    }
  }
  emit loading();
}

void confData::save()
{
  QFile data(dataFilename);
  saveSynchronized(dataFilename);
}

void confData::saveAs(QString fileName)
{
  QFile data(fileName);
  if(!data.open(QIODevice::WriteOnly | QIODevice::Text)) return;
  for(int i=0;i<header.size();i++)
  {
    data.write((header[i] + '\n').toAscii());
  }


  for(int i=0;i<sections.size();i++)
  {
    data.write("#=============================================================================\n",79);
    data.write(("# SECTION:" + sections[i]->title() + "\n").toAscii());
    data.write("#=============================================================================\n",79);
    data.putChar('#'); data.putChar('\n');
	
	for(unsigned int j=0;j<sections[i]->size();j++)
	{
	  confElement *e=(*sections[i])[j];
	  for(int k=0;k<valueSearch.size();k++)
	  {
	    QString v = e->get(valueSearch[k]);
        if(!v.isEmpty())
        {
            v = v.trimmed();
            data.write(("# " + valueSearch[k] + ": " + v + "\n").toAscii());
        }
	  }
	  data.write(("set " + e->get("valuelabel") + " = " + '"' + e->get("value") + '"' + "\n#\n").toAscii());
	}
  }

  data.write("#\n#=============================================================================\n#\n",83);
  setModified(false);
  emit saving();
  data.close();
}

void confData::saveSynchronized(QString fileName)
{
  QFile data(fileName);
  if(!data.open(QIODevice::WriteOnly | QIODevice::Text)) return;
  for(int i=0;i<header.size();i++)
  {
    data.write((header[i] + '\n').toAscii());
  }

  bool synchronized = false;

  for(int i=0;i<sections.size();i++)
  {
    data.write("#=============================================================================\n",79);
    data.write(("# SECTION:" + sections[i]->title() + "\n").toAscii());
    data.write("#=============================================================================\n",79);
    data.putChar('#'); data.putChar('\n');

	for(unsigned int j=0;j<sections[i]->size();j++)
	{
	  confElement *e=(*sections[i])[j];
	  for(int k=0;k<valueSearch.size();k++)
	  {
	    QString v = e->get(valueSearch[k]);
        if(!v.isEmpty())
        {
            v = v.trimmed();
            data.write(("# " + valueSearch[k] + ": " + v + "\n").toAscii());
            if(valueSearch[k]=="SYNC_WITH_UPPER_LEVEL" && v.toLower() == "yes" )
            {
                if(saveInUpperLevel(e->get("valuelabel"),e->get("value")))
                    synchronized = true;
            }
        }
	  }
	  data.write(("set " + e->get("valuelabel") + " = " + '"' + e->get("value") + '"' + "\n#\n").toAscii());
	}
  }

  data.write("#\n#=============================================================================\n#\n",83);
  setModified(false);
  emit saving();
  data.close();
  if(synchronized)
      parentConf->save();
}

void confData::setSaveName(QString fileName)
{
  dataFilename = fileName;
}

confElement* confData::get(QString element)
{
  return lookup[element.toLower()];
}

QString confData::get(QString element, QString value)
{
  if(lookup[element.toLower()]!=NULL)
    return lookup[element.toLower()]->get(value);
  else
    return "";
}

int confData::set(QString element, QString value)
{
  if(lookup[element.toLower()]==NULL) return 0;

  //bool isModified = false;
  if(lookup[element.toLower()]->get("locked").toLower()!="yes")
  {
    if(lookup[element.toLower()]->get("iswrong").toLower().trimmed() == "yes" )
    {
      lookup[element.toLower()]->set("iswrong","NO");
      //isModified = true;
    }
    if(lookup[element.toLower()]->get("value") != value)
    {
      lookup[element.toLower()]->set("value",value);
      //isModified = true;
      setModified(true);
    }
    //if(isModified) setModified(true);
  }

  return 1;
}

QString confData::getDir(QString dir)
{
  return QDir(directories[dir.toLower()]).canonicalPath() + "/";
}

bool confData::setDir(const QString &dirName, const QDir &directory)
{
  directories.insert(dirName.toLower(),directory.canonicalPath());
  if(!directory.exists()) {cerr<<dirName.toStdString()<<": "<<directory.path().toStdString()<<" does not exist."<<endl; return false;}
  else return true;
}

bool confData::addApp(const QString &name, const QString &location)
{
  applications[name.toLower()] = location;
  return true;
}

void confData::setAppConf(confData *conf)
{
  subConfs.insert("appconf",conf);  
}

confData *confData::getSubConf(QString sub)
{
  QHash<QString,confData*>::const_iterator i = subConfs.find(sub.toLower());
  if(i!=subConfs.end() && i.key()==sub.toLower())
  {
    return i.value();
  }
  else
    return NULL;
}

bool confData::setApp(const QString &name, const QString &location)
{
  return addApp(name,location.trimmed());
}

QString confData::getApp(QString app)
{
  QHash<QString,confData*>::const_iterator i = subConfs.find("appconf");
  if(i!=subConfs.end() && i.key()=="appconf")
  {
		confData *d = i.value();
    QString r = d->get(app,"value");
    if(r.isEmpty())
			return applications[app.toLower()]; 
    else return r;
  }
  else if(hasParent() && applications[app.toLower()].isEmpty())
    return parentConf->getApp(app);
  else
    return applications[app.toLower()];
}

void confData::addIcon(const QString &iconName, QIcon *icon)
{
  icons.insert(iconName.toLower(),icon);
}

QIcon *confData::getIcon(QString icon)
{
  if(icons[icon.toLower()] == NULL && parentConf != NULL) return parentConf->getIcon(icon);

  return icons[icon.toLower()];
}

void confData::addImage(const QString &imageName, QImage *image)
{
  images.insert(imageName.toLower(),image);
}

QImage *confData::getImage(QString imageName)
{
  return images[imageName.toLower()];
}

confData *confData::getParent()
{
  return parentConf;
}

void confData::setAppDir(QString dir)
{
  if(dir[dir.size()-1] != '/') dir+='/';
  directories["appdir"] = dir;
  directories["2dx_bin"] = dir + "2dx_bin/";
  directories["bindir"] = dir + "bin/";
  directories["templatedir"] = dir + "scripts-standard/";
  directories["customdir"] = dir + "scripts-custom/";
  directories["procdir"] = dir + "proc/";

  initializationScriptBaseName =  "2dx_initialize";
  initializationScriptName = initializationScriptBaseName + ".script";

  QString logBrowser = "2dx_logbrowser";
  if(QFileInfo(getDir("2dx_bin") + logBrowser).exists())
    applications["logbrowser"] = getDir("2dx_bin") + logBrowser;
  else if(QFileInfo(directories["2dx_bin"] + logBrowser + ".app/" + OS_X_APP_PATH + logBrowser).exists())
    applications["logbrowser"] = getDir("2dx_bin") + logBrowser + ".app/" + OS_X_APP_PATH + logBrowser;

  QString thisApp = QApplication::applicationFilePath();
  if(QFileInfo(thisApp).exists())
    applications["this"] = thisApp;
  else if(QFileInfo(thisApp + ".app/").exists())
    applications["this"] = thisApp + ".app/" + OS_X_APP_PATH + thisApp;

  QImage *appImage = new QImage(getDir("appDir") + "resource/" + "icon.png");
  if(appImage->isNull()) cout<<(getDir("appDir") + "resource/" + "icon.png").toStdString()<<" does not exist"<<endl;
  addImage("appImage", appImage);
}

void confData::setWorkingDir(QString dir)
{
  if(dir[dir.size()-1] != '/') dir+='/';
  directories["workingdir"] = dir;
  directories["workingproc"] = dir + "proc/";
}

QStringList &confData::manual()
{
  return manualData;
}

const QString & confData::initializationScript( bool fullName)
{
  if(fullName)
    return initializationScriptName;
  else
    return initializationScriptBaseName;
}

QStringList confData::getValueSearch()
{
  return valueSearch;
}

void confData::setModified(bool isModified)
{
  modified = isModified;
  if(modified && autoSave)
    save();

  emit dataModified(modified);
}

void confData::setAutoSave(bool value)
{
  autoSave=value;
}

bool confData::isModified()
{
  return modified;
}

const QString &confData::version()
{
  return VERSION_2DX;
}

confData *confData::userConf()
{
  return userData;
}

void confData::setURL(const QString &name, const QString &url)
{
  urls[name.toLower()] = url;
}

const QString &confData::getURL(const QString &name)
{
  return urls[name.toLower()];
}

void confData::setDefaults(const QString &workingDirName)
{
  QString appDir = QApplication::applicationDirPath();
  QString sep = "/../";
#ifdef Q_WS_MAC
  appDir+="/../../../";
#endif
  int tries = 0;
  while(!QFileInfo(appDir + sep + "config/2dx_master.cfg").exists() && tries<3)
  {
    cout<<(appDir + sep + "config/2dx_master.cfg").toStdString()<<" does not exist!"<<endl;
    sep+="../";
    tries++;
  }
  QDir workingDir(workingDirName);
  if(!workingDir.exists()) {qDebug()<<(workingDirName + " does not exist!"); exit(0);}
  workingDir.mkdir("proc");
  workingDir.mkdir("LOGS");
  QString referenceConf = appDir + sep + "config/2dx_master.cfg";
  QString localConf = workingDir.absolutePath() + "/" + "2dx_image.cfg";
  QDir standardScriptsDir(appDir + "/" + "scripts-standard/");
  QDir customScriptsDir(appDir + "/" + "scripts-custom/");
  QDir working(QDir(workingDirName).absolutePath());
  QDir remoteProc(working.path() + "/proc/");

  init(referenceConf);
  confData local(localConf);
  loadConf(&local);
  setSaveName(localConf);

  if(!standardScriptsDir.exists()) {cout<<standardScriptsDir.absolutePath().toStdString()<<" does not exist!"<<endl; exit(0);}
  if(!customScriptsDir.exists()) {cout<<standardScriptsDir.absolutePath().toStdString()<<" does not exist!"<<endl; exit(0);}

  setDir("working",workingDir);
  setDir("binDir",appDir + "/bin");
  setDir("procDir",appDir + "/../proc");
  setDir("standardScriptsDir", standardScriptsDir);
  setDir("customScriptsDir", customScriptsDir);
  setApp("2dx_image",appDir + "../bin/2dx_image");
  setApp("2dx_merge",appDir + "../bin/2dx_merge");
  syncWithUpper();
  if(!QFileInfo(localConf).exists())
  {
    loadDefaults();
    save();
  }
}

QString confData::property(const QString &propertyName)
{
  return properties.value(propertyName.toLower());
}

QList<QString> confData::propertyList(const QString &propertyName)
{
  return properties.values(propertyName.toLower());
}

QList<confElement*> confData::find(const QString &field, const QRegExp &exp)
{
  QList<confElement *> selection;
  QHashIterator<QString,confElement*> it(lookup);
  while(it.hasNext())
  {
    it.next();
    if(it.value() == NULL) cerr<<it.key().toStdString()<<" is somehow empty."<<endl;
    else if(it.value()->get(field).contains(exp))
      selection<<it.value();
  }
  return selection;
}

bool confData::sync(const QString &reference, const QString &variable, const QRegExp &exp)
{
  if(!QFileInfo(reference).exists()) {cerr<<(reference + " does not exist.").toStdString()<<endl; return false;}

  confData local(reference);
  if(local.isEmpty()) {cerr<<(reference + " is empty.").toStdString()<<endl; return false;}

  QRegExp search;

  if(exp.isEmpty())
  {
    search.setPattern("[yY][eE][sS]");
    search.setCaseSensitivity(Qt::CaseInsensitive);
  }
  else search = exp;

  confElement *l;

  foreach(confElement *e, local.find(variable,search))
  {
    l=get(e->get("valuelabel"));
    if(l!=NULL)
      *l = *e;
    else
      cout<<"Could not synchronize variable: "<<e->get("valuelabel").toStdString()<<endl;
  }

  return true;
}

bool confData::syncPropertyWithUpper( const QString element, const QString property)
{
	return syncProperty(QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg",element,property);
}

bool confData::syncWithUpper(const QString &variable, const QRegExp &exp)
{
  return sync(QFileInfo(dataFilename).absolutePath() + "/../2dx_master.cfg",variable,exp);
}

bool confData::syncProperty(const QString reference, const QString element, const QString property)
{
	  //if(!QFileInfo(reference).exists()) {cerr<<(reference + " does not exist.").toStdString()<<endl; return false;}

	  if(!parentConf) {cerr<<"parent configuration does not exist."<<endl; return false;}

	  confData* upper = parentConf;
	  //if(upper.isEmpty()) {cerr<<(reference + " is empty.").toStdString()<<endl; return false;}

	  confElement* upperElement = upper->get(element);
	  confElement* localElement = get(element);
	  //qDebug() << "saving property " << property << " = " << localElement->get(property) << " of element " << element;
	  upperElement->set(property, localElement->get(property));
	  upper->save();

	  return true;
}
const QSet<QString> &confData::subScripts()
{
  return subScript;
}


