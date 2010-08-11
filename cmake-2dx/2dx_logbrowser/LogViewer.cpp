/***************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory   *
 *   HStahlberg@ucdavis.edu   *
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
#include "LogViewer.h"  // header file for the this viewer class
/***************************************************************************
 * This function is the default constructor for the viewer widget class.   *
 * It initializes the widget and creates the viewer as a text display area *
 * for displayin all of the content in the log files.  It also sets the    *
 * layout for the viewer and creates the underlying log file reader.  It   *
 * also stores the verbosity level internally.                             *
 ***************************************************************************/
LogViewer::LogViewer(QString file_name, VerbosityLevel level, QWidget *parent) : QWidget(parent)
{
  text_area_ptr = new QTextEdit(this);  // create the text edit widget with scrolling capabilities
  text_area_ptr->setReadOnly(true);  // set the text edit property to read only
 
  /* set the font size of the log file text to be courier with 10 points and light font */
  text_font = new QFont;  // create the font
  text_font->setFamily("Courier");  // set the font to courier
  text_font->setPointSize(10);  // set the font to 10 point
  text_font->setWeight(QFont::Light);  // set the font to light
  setFont(*text_font);  // set the created font for this text viewer widget

  log_file_name = file_name;  // store the log file name
  currentVLevel = level;  // set the verbosity level to level

  text_document_ptr = text_area_ptr->document();  // get the pointer to the underlying text document
  reader = new LogFileReader(log_file_name, currentVLevel);  // create the log file reader
  reader->setTextDocument(text_area_ptr, text_document_ptr);  // set the readers text document to the viewer document

  viewer_layout = new QGridLayout(this);  // create a new grid layout for the viewer widget
  viewer_layout->addWidget(text_area_ptr, 0, 0, 1, 1);  // add the viewer widget to the layout
  setLayout(viewer_layout);  // set the new layout
}

/***************************************************************************
 * This function turns on the log file reader to start reading the log     *
 * files that are associated with the viewer.                              *
 ***************************************************************************/
void LogViewer::startLogRead()
{
  reader->startRead();  // start reading the log file with the reader
}

/***************************************************************************
 * This function updates the log file text in the viewer by reading new    *
 * data from the log file if any and updates the viewer output.  They are  *
 * both done through interfaces provided by the log file reader class.     *
 ***************************************************************************/
void LogViewer::updateViewer()
{
  reader->readLogData();  // read log file data from the associated log file
  reader->updateOutput();  // update the viewer output
}

/***************************************************************************
 * This function changes the current verbosity level associated with the   *
 * viewer and log file.  It will then call the reader to change its own    *
 * verbosity level to the new one.  In the process, it will update viewer. *
 ***************************************************************************/
void LogViewer::verbosityChange(VerbosityLevel verb_level)
{
  currentVLevel = verb_level;  // change the current verbosity level to new level
  reader->changeVerbosity(currentVLevel);  // tell the reader to change its verbosity level
}

/***************************************************************************
 * This function is the default destructor that does garbage collection    *
 * and clean up.  It delets the reader, document, and text display widget. *
 ***************************************************************************/
LogViewer::~LogViewer()
{ 
  if(reader!=NULL)  // if reader exists
  {
    delete reader;  // delete the reader
  }

  if(text_document_ptr!=NULL)  // if text document exists
  {
    delete text_document_ptr;  // delete the document
  }

  if(text_area_ptr!=NULL)  // if text display area viewer exists
  {
    delete text_area_ptr;  // delete the text display area viewer
  }
}
