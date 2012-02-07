/***************************************************************************
*   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
*   HStahlberg@ucdavis.edu                                                *
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This program is distributed in the hope that it will be useful,       *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*   GNU General Public License for more details.                          *
*                                                                         *
*   You should have received a copy of the GNU General Public License     *
*   along with this program; if not, write to the                         *
*   Free Software Foundation, Inc.,                                       *
*   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
***************************************************************************/

#ifndef CONFDATA_H
#define CONFDATA_H

#include <QApplication>
#include <QList>
#include <QMultiHash>
#include <QSet>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QString>
#include <QIcon>
#include "confElement.h"
#include "confSection.h"

const QString VERSION_2DX="3.3.0";

class confData;

class confData : public QObject
{
  Q_OBJECT

  public slots:
  void save();
  void saveAs(QString fileName);
  void saveSynchronized(QString fileName);
  void setSaveName(QString fileName);
  void load();
  void reload();
  void loadConf(confData *conf);
  void loadDefaultConf(confData *conf, const QStringList &defaults);
  void setModified(bool isModified);
  void setAutoSave(bool value);
  void loadDefaults();
  bool syncWithUpper(const QString &variable = "SYNC_WITH_UPPER_LEVEL", const QRegExp &exp = QRegExp());


  void setAppConf(confData *conf = NULL);

  confElement* get(QString element);
  QString get(QString element, QString value);

  signals:

  void dataModified(bool);
  void loading();
  void saving();

  private:
  QStringList header;
  QList<confSection *> sections;
  QHash<QString,confElement *> lookup;
  QHash<QString,QString> directories; //appDir, binDir, templateDir, workingDir, procDir;
  QHash<QString,QString> applications; //logBrowser
  QHash<QString,QIcon *> icons;
  QHash<QString,QImage *> images;
  QHash<QString,QString> urls;
  QHash<QString,confData*> subConfs;
  QSet<QString> subScript;
  QMultiHash<QString,QString> properties;
  confData *parentConf;
  confData *userData;
  QString dataFilename;
  QString linkName;
  QStringList valueSearch;
  QStringList userSetProperties;
  QStringList globalSetProperties;
  QStringList manualData;
  QString initializationScriptBaseName;
  QString initializationScriptName;
  bool empty;
  bool modified;
  bool autoSave;

  QString OS_X_APP_PATH;

  void verifyElement(confSection *&section, confElement *&e);
  confSection* getSection(const QString title);
  confElement* getElement(confElement* e);
  confElement* protectUserSetProperies(confElement* source, confElement* target);
  void init(const QString &fileName, confData *parentData = NULL);

  QString readLine(QFile &data);
  QString &parseVariables(QString &line);
  bool saveInUpperLevel(QString variable,QString value);
  QString printLookup();
  bool syncProperty(const QString reference, const QString element, const QString property = "SYNC_WITH_UPPER_LEVEL");


  public:
  confData(QString filename, confData *parentData = NULL, QObject *parent = NULL);
  confData(QString filename, confData *parentData, const QString link, QObject *parent = NULL);
  confData(const confData &data);
  confData(const QString &source, const QString &reference);
  confData(const QString &source, const QString &reference, const QString &link);
  confData(QObject *parent = NULL);
  ~confData();

  bool sync(const QString &reference, const QString &variable = "SYNC_WITH_UPPER_LEVEL", const QRegExp &exp = QRegExp());
  bool syncPropertyWithUpper( const QString element, const QString property = "SYNC_WITH_UPPER_LEVEL");
  bool isEmpty();
  bool isModified();
  bool hasParent();

  void setAppDir(QString dir);
  void setWorkingDir(QString dir);
  void setParentConf(confData *parent);
  void setUserConf(confData *userConf);
  void setSymLink(const QString fileName, const QString linkName);

  void clear();

  int set(QString element, QString value);

  QString getDir(QString dir);
  QString getDataFilename(){ return dataFilename;}
  bool setDir(const QString &dirName, const QDir &directory);
  bool addApp(const QString &name, const QString &location);
  bool setApp(const QString &name, const QString &location);
  QString getApp(QString app);
  void addIcon(const QString &iconName, QIcon *icon);
  QIcon *getIcon(QString icon);
  void addImage(const QString &imageName, QImage *image);
  QImage *getImage(QString imageName);
  confData *getStatusData();
  confData *getParent();
  confData *userConf();

  QString property(const QString &propertyName);
  QList<QString> propertyList(const QString &propertyName);

  QStringList &manual();
  const QSet<QString> &subScripts();

  const QString & initializationScript(bool fullName = true);

  bool parseDataFile();
  void updateConf(const QString &confFileName);
  unsigned int size();
  QList<confSection *> getSections();
  void insertSections(QList<confSection *> sectionList);
  QHash<QString,confElement *> getLookupTable();
  QStringList getValueSearch();
  confSection* operator[](unsigned int i);
  const confSection* operator[](unsigned int i) const;

  const QString &version();
  void setURL(const QString &name, const QString &url);
  const QString &getURL(const QString &name);

  void setDefaults(const QString &workingDirName);

  QList<confElement*> find(const QString &field, const QRegExp &exp);
	confData *getSubConf(QString sub);

};

#endif
