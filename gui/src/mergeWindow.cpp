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

#include "mergeWindow.h"

using namespace std;

mergeWindow::mergeWindow(confData* conf, QWidget *parent)
: QWidget(parent) {

    mainData = conf;

    results = new resultsData(mainData, mainData->getDir("working") + "/LOGS/" + "2dx_initialization.results", mainData->getDir("working"), this);
    albumCont = new libraryContainer(mainData, results, this);
    executionCont = new executionContainer(mainData, results, this);

    connect(executionCont, SIGNAL(scriptCompletedSignal()), albumCont, SLOT(maskResults()));
    connect(albumCont->getDirView(), SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(itemActivated(const QModelIndex&)));

    QSplitter* container = new QSplitter(Qt::Vertical, this);
    
    container->addWidget(albumCont);
    container->addWidget(executionCont);
    container->setStretchFactor(0, 1);
    container->setStretchFactor(1, 2);

    QGridLayout* layout = new QGridLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addWidget(container);

    setLayout(layout);
    
    container->setSizes(QList<int>() << 1 << 2);

}

void mergeWindow::imageLibraryDisplayed(bool show) {
    albumCont->showContents(show);
}

void mergeWindow::showSelected(bool show) {
    albumCont->showSelected(show);
}

projectModel* mergeWindow::getDirModel() 
{
    return albumCont->getDirModel();
}

QTreeView* mergeWindow::getDirView() 
{
    return albumCont->getDirView();
}

void mergeWindow::reload() {
    results->load();
    albumCont->reload();
}

void mergeWindow::loadDirectorySelection() {
    QString loadName = QFileDialog::getOpenFileName(this, "Save Selection As...", mainData->getDir("working") + "/2dx_merge_dirfile.dat");
    albumCont->loadSelection(loadName);
}

void mergeWindow::itemActivated(const QModelIndex& index){
    emit imageActivated(albumCont->getDirModel()->pathFromIndex(index));
}