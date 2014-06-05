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

#include <iostream>
#include <imageAlbum.h>
#include <QDebug>
using namespace std;

imageAlbum::imageAlbum(const projectModel *dirModel, QWidget *parent)
          :QWidget(parent,Qt::Window)
{
	QGridLayout *layout = new QGridLayout(this);
	layout->setMargin(0);
	layout->setSpacing(0);
	setLayout(layout);

	model = new albumModel(dirModel,this);
	view = new QListView(this);
	view->setViewMode(QListView::IconMode);
	view->setSpacing(30);
	view->setGridSize(QSize(120,120));
	view->setIconSize(QSize(100,100));
	view->setMovement(QListView::Static);
	view->setResizeMode(QListView::Adjust);
	view->setFlow(QListView::LeftToRight);
	view->setLayoutMode(QListView::SinglePass);	
	view->setModel(model);
	//view->setItemDelegate(new albumDelegate);
	
	connect(view->selectionModel(),SIGNAL(currentChanged(const QModelIndex&,const QModelIndex&)),this,SLOT(setConf(const QModelIndex&)));
	connect(view->selectionModel(),SIGNAL(currentChanged(const QModelIndex&,const QModelIndex&)),this,SLOT(viewImage(const QModelIndex&)));
	
	selection = new QItemSelectionModel(model);
	viewer = new albumViewer;

	QSplitter *albumSplitter = new QSplitter(Qt::Vertical);
	albumSplitter->addWidget(view);
	albumSplitter->addWidget(viewer);

	selectionWidget = new QListWidget;
	
	viewContainer *selectorContainer = new viewContainer("Selection", viewContainer::image, this);
	selectorContainer->addWidget(selectionWidget);
	
	QSplitter *splitter = new QSplitter(Qt::Horizontal);
	splitter->addWidget(selectorContainer);
	splitter->addWidget(albumSplitter);
	splitter->setStretchFactor(0,1);
	splitter->setStretchFactor(1,3);

	layout->addWidget(splitter);

	resize(805,768);
	setWindowTitle("Reconstruction Album");
	show();
	
	connect(this,SIGNAL(confSelected(const QString &)),viewer,SLOT(setConf(const QString &)));
	connect(this,SIGNAL(imageSelected(const QString &)),viewer,SLOT(setImage(const QString &)));
}

void imageAlbum::viewImage(const QModelIndex &index)
{
	emit imageSelected(index.data(Qt::UserRole + 1).toString());
}

void imageAlbum::setConf(const QModelIndex &index)
{
	QString confName = QFileInfo(index.data(Qt::UserRole + 1).toString()).path() + "/" + "2dx_image.cfg";
	
	if(QFileInfo(confName).exists())
	{
		emit confSelected(confName);
	}
	else
	{
		qDebug()<<confName<<" not found!"<<endl;
	}
	
	updateParameterSection(confName);
}

void imageAlbum::updateParameterSection(QString confName)
{
	confData* data = new confData(confName);
	QString imagename = data->get("imagename", "value");
	
	selectionWidget->clear();
	
	selectionWidget->addItem("Name: " + data->get("imagename", "value"));
	selectionWidget->addItem("Number: " + data->get("imagenumber", "value"));
	selectionWidget->addItem("QVal2: " + data->get("QVAL2", "value"));
	selectionWidget->addItem("QValS: " + data->get("QVALS", "value"));
	selectionWidget->addItem("Phase Residual: " + data->get("MergePhaseResidual", "value"));
	selectionWidget->addItem("TAXA: " + data->get("TAXA", "value"));
	selectionWidget->addItem("TANGL: " + data->get("TANGL", "value"));
	selectionWidget->addItem("Magnification: " + data->get("magnification", "value"));
	selectionWidget->addItem("Defocus: " + data->get("defocus", "value"));
	selectionWidget->addItem("Comment: " + data->get("comment", "value"));
	
	
	//QString imagenumber = data->get("imagenumber", "value");
	//QListWidgetItem* it = selectionWidget->item(2);
	//QString text = QString("asdgf");
	//it->setText(text);
  
	

}

void imageAlbum::reload()
{
	model->reload();
}

void imageAlbum::setModel(QAbstractItemModel *model)
{
	view->setModel(model);
	view->setRootIndex(model->index(1,0));
	view->setModelColumn(2);
}

void imageAlbum::setSelectionModel(QItemSelectionModel *selectionModel)
{
	view->setSelectionModel(selectionModel);
}

void imageAlbum::currentSelectionChanged(const QModelIndex &current,const QModelIndex &previous)
{
	qDebug()<<current.data(Qt::UserRole+5).toString();
}
