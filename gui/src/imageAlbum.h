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

#ifndef IMAGEALBUM_H
#define IMAGEALBUM_H

#include <QWidget>
#include <QGridLayout>
#include <QListView>
#include <QListWidget>
#include <QSplitter>
#include <albumModel.h>
#include <albumDelegate.h>
#include <viewContainer.h>
#include <albumViewer.h>

class imageAlbum : public QWidget
{
	Q_OBJECT
	
	public slots:
		void viewImage(const QModelIndex &index);
		void setConf(const QModelIndex &index);
		void setModel(QAbstractItemModel *model);
		void setSelectionModel(QItemSelectionModel *selectionModel);
		void currentSelectionChanged(const QModelIndex &current,const QModelIndex &previous);  
  
	signals:
		void imageSelected(const QString &imageName);
		void confSelected(const QString &confFile);

	private:
		albumModel *model;
		QListView *view;
		
		QListWidget *selectionWidget;

		QItemSelectionModel *selection;
		albumViewer *viewer;
		
		void updateParameterSection(QString confName);

	public:
		imageAlbum(const projectModel *dirModel, QWidget *parent = NULL);
		void reload();  
};

#endif

