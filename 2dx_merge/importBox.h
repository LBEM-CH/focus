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

#ifndef IMPORTBOX_H
#define IMPORTBOX_H

#include <QDialog>
#include <QGridLayout>
#include <ui_importBox.h>
#include <imageNameParser.h>

class importBox : public QDialog
{
  Q_OBJECT

  public slots:
    void acceptImage();
    void accept();
    
  signals:
    void acceptedImage(const QHash<QString,QString> &imageCodes);
  
  private:
    QHash<QString,QString> imageCodesList;
    Ui::Dialog ui;

  public:
    importBox(const QString &imageName, QWidget *parent = NULL, Qt::WindowFlags f = 0);
    const QHash<QString, QString> imageCodes();
    
  protected:
      void hide();
  
};

#endif
