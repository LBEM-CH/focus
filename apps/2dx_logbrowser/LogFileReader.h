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
#ifndef LOGFILEREADER_H  // conditional compilation for protection
#define LOGFILEREADER_H

#include <qtextedit.h>  // qt text editor area class library
#include <qobject.h>  // qt object class library
#include <qstring.h>  // qt string class library
#include <qstringlist.h>  // qt string list class library
#include <qiodevice.h>  // qt I/O device class library
#include <qfile.h>  // qt file interface class library
#include <qtextdocument.h>  // qt text document interface class library
#include <qtextcursor.h>  // qt text cursor interface class
#include <qchar.h>  // qt character interface class

#include "LogBrowserCommons.h"  // common definitions for the project

class LogFileReader;  // forward declaration
/***************************************************************************
 * This class implements a file reader.  It implements all log file I/O's  *
 * in the class and it contains a text document and cursor so as the class *
 * reads data from the log files, it will update it.  Essentially, it is a *
 * container for file I/O and output of data to the viewer screen.         *
 ***************************************************************************/
class LogFileReader
{
    private:
    qint64 total_num_bytes;  // total number of bytes available from last access
    qint64 num_bytes_read;  // number of bytes from log file already read
    qint64 num_lines_read;  // number of lines of text read from log file 
    qint64 num_lines_parsed;  // number of lines parsed and outputted to the screen viewer
    QString log_file_name;  // the name of the log file to read

    QFile *log_file_ptr;  // qt file pointer to the log file
    
    QTextEdit *output_text_area;  // text area that contains the document and cursor
    QTextDocument *output_text_document;  // text document that contains the text of log file
    QTextCursor *cursor_ptr;  // cursor interface to add text to the viewer
    QStringList log_file_text;  // string list containing all the lines of text in the log file
    QString temp_read_str_buffer;  // buffer used for temporary storage of line of text read from log file

    VerbosityLevel currentVLevel;  // the current verbosity level used for displaying the log file text

    void reOutputLog();  // redisplay the entire content of the log file in viewer with the given verbosity level
    void outputNewText(QString & str_buf);  // output a new line of text to the viewer 
    void makeNewCursor();  // create a new cursor to interface with the viewer

    public:
    LogFileReader(QString file_name="", VerbosityLevel level=level_one);  // constructor with viewer name and log file name
    ~LogFileReader();  // destructor that cleans up text in memory and allocated objects
    void setTextDocument(QTextEdit *text_area, QTextDocument *text_doc);  // set the text document and area the device will be streaming the data to
    void setVerbosityLevel(VerbosityLevel new_level);  // set the verbosity level of the viewer but does not reoutput to viewer
    void startRead();  // start reading the log file
    void changeVerbosity(VerbosityLevel level);  // change the verbosity level of the viewer and reoutputs to log files to viewer
    void readLogData();  // read all of the unread data from log files to internal memory kept string list
    void updateOutput();  // update the viewer output
};

#endif
