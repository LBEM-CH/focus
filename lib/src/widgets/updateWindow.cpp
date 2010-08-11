/**************************************************************************
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


#include "updateWindow.h"
#include <iostream>
using namespace std;

updateWindow::updateWindow(confData *conf, QWidget *parent)
             :QWidget(parent,Qt::Window)
{

  QAction *hideWindow = new QAction(tr("Close Window"),this);
  hideWindow->setShortcut(tr("Ctrl+W"));
  addAction(hideWindow);
  connect(hideWindow,SIGNAL(triggered()),this,SLOT(hide()));

  setWindowTitle("Software Update");

  data = conf;
  installedVersion = data->version();

  setMinimumWidth(600);
  //setFixedSize(QSize(568,410));

  QGridLayout *layout = new QGridLayout(this);
  setLayout(layout);


  if(data->getImage("appImage")!=NULL)
  {
    QLabel *appImage = new QLabel();
    appImage->setPixmap(QPixmap::fromImage(*(data->getImage("appImage"))));
    layout->addWidget(appImage,0,0,3,1);
  }
  else
    cout<<"appImage not found"<<endl;

  updateTitle = new QLabel("2dx release information.");
  updateTitle->setFont(QFont("Times",18,QFont::Bold));
  layout->addWidget(updateTitle,0,1,1,3);

  versionInfo = new QLabel();
  versionInfo->setWordWrap(true);
  versionInfo->setFont(QFont("Times",14));
  layout->addWidget(versionInfo,1,1,1,3);

  QLabel *releaseInfo = new QLabel("Release Information: ");
  releaseInfo->setFont(QFont("Times",14,QFont::Bold));
  layout->addWidget(releaseInfo,2,1,1,3);

  updateText = new QTextBrowser(this);
  updateText->setFont(QFont("Times",12));
  layout->addWidget(updateText,3,1,1,3);

  QPushButton *upgradeButton = new QPushButton("Update Version");
  layout->addWidget(upgradeButton,4,3,1,1);
  connect(upgradeButton,SIGNAL(clicked()),this,SLOT(updateVersion()));

  updateInf = new QHttp("www.2dx.org");
  connect(updateInf,SIGNAL(requestFinished(int,bool)),this,SLOT(updateTextBox()));
  updateInf->get("/download/2dx-software/2dx-installer/changes.htm");

}

void updateWindow::updateTextBox()
{
  if(updateInf->error())
  {
    cout<<"Http error: "<<updateInf->errorString().toStdString()<<endl;
    versionInfo->setText("No version information currently available.");
    updateText->insertPlainText(updateInf->errorString());
    return;
  }
  QString updateString = updateInf->readAll();
  QString currentVersion = updateString.section("##",1,1).trimmed();
  QString remindUpdate = data->userConf()->get("remindUpdate","value").toLower();
  QString intVersion = currentVersion, intInstalled = installedVersion;
  intVersion.remove('.'); intInstalled.remove('.');
  if(intVersion.toInt()>intInstalled.toInt())
  {
    updateTitle->setText("A new version of 2dx is available.");
    versionInfo->setText("2dx-" + currentVersion + " is now available, your current version is 2dx-" + installedVersion + ".<br>Would you like to upgrade?<br>");
    if(remindUpdate!=currentVersion)
    {
      int choice = QMessageBox::question(this,tr("New version available"),tr("A new version of 2dx is available. <br> Would you like to upgrade?"),tr("Tell Me More"),tr("Skip this version"),tr("Not now"));
      if(choice == 0) show();
      if(choice == 1) {data->userConf()->set("remindUpdate",currentVersion); data->userConf()->save();}
    }
  }
  else
    versionInfo->setText("The currently installed version, 2dx-" + installedVersion + " is the latest available.");
  updateText->insertHtml(updateString);
  updateText->moveCursor(QTextCursor::Start);
}

void updateWindow::updateVersion()
{
  QProcess::startDetached(data->getApp("webBrowser") + " " + "http://2dx.org/download/2dx-software/");
}
