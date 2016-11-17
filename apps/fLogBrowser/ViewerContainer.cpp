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
#include "ViewerContainer.h"  // viewer container class header
/***************************************************************************
 * This function is the default constructor for the container widget class *
 * that contains all of the individual viewer widgets associated with all  *
 * of the individual log files.  The constructor is a child of the main    *
 * window dialog box and receives as arguments the directory where all of  *
 * the log files are located and the verbosity level associated with all   *
 * of the log files.                                                       *
 ***************************************************************************/
ViewerContainer::ViewerContainer(QWidget *parent, QString log_directory, QString first_file, VerbosityLevel level) : QTabWidget(parent)
{
  int counter;  // temporary counter used to run through directory list loops
  bool directory_exists;  // flag indicating whether the log file directory given exists
  QString first_file_name;  // file name of the log file that is to be 
  QFile *first_file_ptr;  // file object for storing the default file to be the current widget
  QFileDialog *directory_selection_dialog;  // file directory selection dialog box object
  QStringList temp_dir_list;  // a list of all the directories selected by the user
  QString project_directory_name;  // the name of the directory where the log files are located
  LogViewer *tmp_viewer_ptr;  // temporary log viewer pointer for generating viewers
  QProgressDialog *progress_ptr; // a progress bar for start up loading
  
  directory_selection_dialog = NULL;
  progress_ptr = NULL;  // progress bar dialog box points to nothing to start with
  vlevel = level;  // store the default verbosity level
  directory_name = log_directory;  // store the directory name where log files are

  QFileInfo info(directory_name);
  if(first_file.isEmpty() && info.isFile()) first_file = info.fileName();
  if(info.isFile()) directory_name = info.absolutePath() + "/";

  directory_ptr = new LogDirectory(directory_name);  // create a new object associated with log directory
  directory_exists = directory_ptr->loadLogFileList();  // load the list of files in the log directory

  if(!directory_exists)  // if directory does not exist then display a critical warning message
  {
    /* critical warning message telling the user that the directory where the log files should be located cannot be found */
    QMessageBox::critical(this, "fLogBrowser", "Cannot find the default directory " + directory_name + " \nPlease select a log file directory",
                          QMessageBox::Ok, QMessageBox::NoButton, QMessageBox::NoButton);

    /* instead of exit, it should prompt for a directory where the log files are */
     directory_selection_dialog = new QFileDialog(this);  // create the file dialog box 
     directory_selection_dialog->setFileMode(QFileDialog::DirectoryOnly);  // set the selection type to directories only
     directory_selection_dialog->show();  // show the dialog box
     directory_selection_dialog->setDirectory(QDir::currentPath());  // set the directory path to the current user path
     directory_selection_dialog->setViewMode(QFileDialog::List);  // set the display mode of the directories to be list only
     directory_selection_dialog->setResolveSymlinks(true);  // set to resolve symbolic links
     directory_selection_dialog->exec();  // create its own event loop for the file dialog box
     temp_dir_list = directory_selection_dialog->selectedFiles();  // get the list of selected directories
     project_directory_name = temp_dir_list.value(0);  // only one directory should be selected, the first one is taken as the selection
     directory_ptr->setNewLogDirectory(project_directory_name);  // set the new directory to be the one selected by the user in the dialog box

    if(project_directory_name.isEmpty())  // if no directory was selected
    {
      QMessageBox::critical(this, "fLogBrowser", "No log file directory selected\nProgram Exiting",
                            QMessageBox::Ok, QMessageBox::NoButton, QMessageBox::NoButton);  // display a no directory selected error message
      exit(0);  // exit the program
    }
    else if(!directory_ptr->loadLogFileList())  // if the directory selected does not exist
    {
      QMessageBox::critical(this, "fLogBrowser", "Cannot open specified directory!\nProgram Exiting",
                            QMessageBox::Ok, QMessageBox::NoButton, QMessageBox::NoButton);  // display an invalid directory error message
      exit(0);  // exit the program
    }
    directory_name = project_directory_name;  // the project directory name is stored internally
  }
  
  file_list = directory_ptr->getLogFileList();  // get the list of log files in the selected directory
  file_list_size = file_list.size();  // get the number of log files in the directory
  progress_ptr = new QProgressDialog("Initializing...",  NULL, 0, file_list_size, this);  // a progress bar for start up loading
  progress_ptr->setMinimumDuration(0);  // set the minimum duration to 0 milliseconds so it will show at start up

  /* if no file exist the loop will not execute and no tab widget will be created, such is the case with non-existent or empty directories */
  for(counter=0;counter<file_list_size;counter++)  /* loop through the string list and create a logviewer for each file */
  {
    /* create a new log viewer widget for every log file */
    tmp_viewer_ptr = new LogViewer(directory_name + "/" + file_list.value(counter), vlevel, NULL);
    addTab(tmp_viewer_ptr, file_list.value(counter));  // add the viewer to the tab widget list
    progress_ptr->setValue(counter);  // set the value of the progress bar to the current numbers of files processed

    /* loop through the entire hash table and change verbosity level of every stored viewer widget */
    QApplication::processEvents();  // process any other events in the message loop for this program
    tmp_viewer_ptr->startLogRead();  // start reading the each individual log file in the directory
    QApplication::processEvents();  // process any other events in the message loop for this program
    viewer_list.insert(file_list.value(counter), tmp_viewer_ptr);  // insert the widget viewer into a hash list
  }

  progress_ptr->setValue(counter);  //set the final progress value when things are done

  if(!first_file.isEmpty())  // if a parameter argument is given for the default start up file
  {
    first_file_ptr = new QFile(directory_name + "/" + first_file);  // create a new log file object
    if(first_file_ptr->exists())  // if the file actually exists
    {
      setCurrentIndex(indexOf(viewer_list.value(first_file)));  // make it into the current tab in the viewer
    }
  }

  /* connect changing tab widgets to update the selected viewer call back slot */
  connect(this, SIGNAL(currentChanged(int)), this, SLOT(updateSelectedViewer(int)));

  if(progress_ptr!=NULL)  // if progress dialog box exists
  {
    delete progress_ptr;  // delete the progress bar dialog box at the end when everything is loaded
  }

  if(directory_selection_dialog!=NULL)
  {
    delete directory_selection_dialog;
  }
}

/***************************************************************************
 * This function changes the verbosity level of the container class and    *
 * will call all of the contained viewer widgets to change their verbosity *
 * levels to the new level in the container.                               *
 ***************************************************************************/
void ViewerContainer::changeVerbosityLevel(int verb_level)
{
  bool verb_change_cancel;  // the cancel flag for canceling verbosity change
  int counter;  // temporary loop counter
  QProgressDialog progress("Changing verbosity level...", "Cancel", 0, file_list_size, this);  // progress bar dialog box initialization

  switch(verb_level)  // decide which verbosity level is selected
  {
    case 1:
         vlevel = level_one;  // verbosity level one
         break;
    case 2:
         vlevel = level_two;  // verbosity level two
         break;
    case 3:
         vlevel = level_three;  // verbosity level three
         break;
    default:
         break;
  }
  
  counter = 0;  // initially set the counter to zero
  verb_change_cancel = false;  // set the cancel button to false since it has not been pressed yet
  progress.setValue(counter);  // set the starting position of progress bar to the number of files processed (aka zero)
  progress.setMinimumDuration(0);  // the minimum duration to show is zero milliseconds so it will show every time

  /* loop through the entire hash table and change verbosity level of every stored viewer widget */
  while((counter<file_list_size)&&(!verb_change_cancel))
  {
    QApplication::processEvents();  // process the applications other events in the event loop

    if(progress.wasCanceled())  // if user canceled the operation
    {
      verb_change_cancel = true;  // set the cancel flag
    }
    else
    {
      viewer_list.value(file_list.value(counter))->verbosityChange(vlevel);  // get widget and change verbosity
      QApplication::processEvents();  // process the applications other events in the event loop again
      counter++;  // increment the number of files processed counter
      progress.setValue(counter);  // set the number of files processed out of total number of log files to be the progress value
    }
  }

  progress.setValue(counter);  //set the final progress value when things are done
}

/***************************************************************************
 * This function is a response to the update button.  It queries the log   *
 * file directory again and find out if any new files have been created or *
 * if any old files have been deleted.  It then generates new viewers and  *
 * deletes old viewers.  This makes the viewer dynamically associated with *
 * the state of the log files.                                             *
 ***************************************************************************/
void ViewerContainer::updateLogViewers()
{
  int counter;  // local temporary counter
  int tmp_file_list_size;  // number of log files in directory now
  int current_tab_index;  // the index number of the currently selected tab widget
  QStringList tmp_file_list;  // string list that stores the current list of files in log directory
  LogViewer *tmp_viewer_ptr;  // temporary log viewer object

  directory_ptr->loadLogFileList();  // load the current list of files in the log directory
  tmp_file_list = directory_ptr->getLogFileList();  // get the list of log files into a string list
  tmp_file_list_size = tmp_file_list.size();  // get the number of log files in the log files directory

 /**************************************************************************
  * Loop that compares the old file list with the current file list since  *
  * last update and see if they match.  If still in old list and no longer *
  * in current list, then the log file has been deleted and thus, the      *
  * program will also delete the viewer associated with it.                * 
  **************************************************************************/
  for(counter=0;counter<file_list_size;counter++)  // loop through the old file list
  {
    if(!tmp_file_list.contains(file_list.value(counter)))  // in the old list but not in new list so delete it since log file no longer exists */
    {
      tmp_viewer_ptr = viewer_list.value(file_list.value(counter));  // get the old viewer associated with the deleted file
      removeTab(indexOf(tmp_viewer_ptr));  // delete the associated tab from the tab widgets
      tmp_viewer_ptr = viewer_list.take(file_list.value(counter));  // get the associated viewer widget from the hash list

      if(tmp_viewer_ptr!=NULL)  // if the viewer exists
      {
        delete tmp_viewer_ptr;  // delete the viewer
      }
    }
  }

 /**************************************************************************
  * Loop that compares the current file list with the old file list since  *
  * last update and see if they match.  If still in current list and no    *
  * longer in old list, then a new log file has been added and thus, the   *
  * program will also add a new viewer associated with it.                 * 
  **************************************************************************/ 
  for(counter=0;counter<tmp_file_list_size;counter++)  // loop through the string list and create a logviewer for each file 
  {
    if(!file_list.contains(tmp_file_list.value(counter)))  // find out if the current file exists and create new viewer if doesn't exist
    { 
      /* create a new viewer widget with the associated file name */
      tmp_viewer_ptr = new LogViewer(directory_name + "/" + tmp_file_list.value(counter), vlevel, NULL);
      addTab(tmp_viewer_ptr, tmp_file_list.value(counter));  // add a new tab for the newly created widget in the tab widget
      tmp_viewer_ptr->startLogRead();  // start reading the associated log file
      viewer_list.insert(tmp_file_list.value(counter), tmp_viewer_ptr);  // insert the new viewer file into hash table
    }
  }

  file_list = tmp_file_list;  // make the current file list the permanent file list until next update
  file_list_size = tmp_file_list_size;  // the current number of log files are internally kept
  current_tab_index = currentIndex();  //get the current tab widget viewer being viewed
  tmp_viewer_ptr = (LogViewer *)widget(current_tab_index);  // get the pointer to the widget

  if(tmp_viewer_ptr!=NULL)
  {
    tmp_viewer_ptr->updateViewer();  // update the widget and thus, updating the viewer
  }
}

/***************************************************************************
 * This function updates the selected viewer widget in the tab widget by   *
 * getting the currently viewed widget and updating the associated viewer. *
 ***************************************************************************/
void ViewerContainer::updateSelectedViewer(int index)
{
  LogViewer *tmp_viewer_ptr;  // temporary viewer pointer

  tmp_viewer_ptr = (LogViewer *)widget(index);  // get the currently selected viewer widget
  tmp_viewer_ptr->updateViewer();  // update the currently selected viewer
}
