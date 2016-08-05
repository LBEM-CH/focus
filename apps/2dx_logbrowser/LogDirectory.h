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
#ifndef LOGDIRECTORY_H  // conditional compilation for protection
#define LOGDIRECTORY_H

#include <qstring.h>  // qt string class library
#include <qdir.h>  // qt directory class library

class LogDirectory;  // forward declaration
/***************************************************************************
 *   This class is the main window dialog box view for the log browser.    *
 *   It contains the viewer container class and the verbosity and update   *
 *   buttons.  It is the class for the primary widget for the application. *
 ***************************************************************************/
class LogDirectory
{
    private:
    QDir *tmp_dir_ptr;  // directory where the log files are located
    QString log_directory_name;  // string contain name of log files directory
    QStringList log_file_list;  // string list that contains names of all log files

    public:
    LogDirectory(QString directory_name="");  // default constructor with directory name as argument
    void setNewLogDirectory(QString directory_name="");  // function to change directory
    bool loadLogFileList();  // function to load the list of log files in directory
    void updateLogFileList();  // function to update the list of log files in directory
    QStringList getLogFileList();  // function to get the list of log files
};

#endif
