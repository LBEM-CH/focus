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
#ifndef VIEWERCONTAINER_H  // conditional compilation for protection
#define VIEWERCONTAINER_H

#include <qapplication.h>  // qt application class library
#include <qdialog.h>  // qt dialog box class library
#include <qtabwidget.h>  // qt tab widget class library
#include <qstring.h>  // qt string class library
#include <qstringlist.h>  // qt string list class library
#include <qlist.h>  // qt list class library
#include <qwidget.h>  // qt widget class library
#include <qhash.h>  // qt hash table class library
#include <qmessagebox.h>  // qt message box class library
#include <qfiledialog.h>  // qt file selection dialog box class library
#include <qprogressdialog.h> // qt progress bar dialog box class library

#include "LogBrowserCommons.h"
#include "LogDirectory.h"
#include "LogViewer.h"

class ViewerContainer;  // forward declaration
/***************************************************************************
 * This class implements the viewer container class that is a tab widget   *
 * class that contains a series of viewer widgets acccessed by the user as *
 * tab views.  They contain all of the viewer widgets, hence a container.  *
 ***************************************************************************/
class ViewerContainer : public QTabWidget
{
    Q_OBJECT  // qt object macro

    private:
    int file_list_size;  // the number of files in the log files directory
    QString directory_name;  // the name of the directory where the log files are located
    QStringList file_list;  // the string list containing names of log files
    QHash<QString, LogViewer *> viewer_list;  // a hash table with log file names and pointers to their viewer widgets
    VerbosityLevel vlevel;  // the current verbosity level for all of the viewers stored in this container class
    LogDirectory *directory_ptr;  // the directory object for the log files

    public:
    ViewerContainer(QWidget *parent=NULL, QString log_directory="", QString first_file="", VerbosityLevel level=level_one);  // default constructor
    
    public slots:
    void changeVerbosityLevel(int verb_level);  // slot call back function to respond to change in verbosity level
    void updateLogViewers();  // slot call back function to update the log viewers with newly appended content
    void updateSelectedViewer(int index);  // slot call back function to update selected log viewers
};

#endif
