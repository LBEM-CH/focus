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
#include "LogDirectory.h"  // header for log directory class
#include <QDebug>
/***************************************************************************
 * This function is the default constructor for the log directory class.   *
 * It simply takes the name of the directory and stores it internally.     *
 * When needed, it will open a directory with the stored name.             *
 ***************************************************************************/
LogDirectory::LogDirectory(QString directory_name)
{
  setNewLogDirectory(directory_name);  // store the directory name internally
}

/***************************************************************************
 * This function allows the user to pass a new directory name to the class *
 * and thus, effectively changing the directory the class keeps internally *
 ***************************************************************************/
void LogDirectory::setNewLogDirectory(QString directory_name)
{
  QFileInfo info(directory_name);
  log_directory_name = info.absolutePath();  //directory_name;  // store the directory name internally
}

/***************************************************************************
 * This function loads the list of files in the directory kept within the  *
 * class.  If directory does not exist, then it will give a message to the *
 * by returning false.                                                     *
 ***************************************************************************/
bool LogDirectory::loadLogFileList()
{
  QDir dir(log_directory_name);
  
  if(dir.exists())
  {
    log_file_list = dir.entryList(QStringList() << "*.log", QDir::Files, QDir::Time);
    return(true);
  }
  else
  {
    return(false);
  }

}

/***************************************************************************
 * This function updates the file list within the directory to see if new  *
 * files have been added to the directory.  It simply calls load function. *
 ***************************************************************************/
void LogDirectory::updateLogFileList()
{
  loadLogFileList();  // calls the load file list function to load new list
}

/***************************************************************************
 * This function returns a qt string list that contains all of the names   *
 * of the filesin the directory contained within the class.  This function *
 * can be used to query all of the existing files within a directory.      *
 ***************************************************************************/
QStringList LogDirectory::getLogFileList()
{
  return(log_file_list);  // returns the file list
}
