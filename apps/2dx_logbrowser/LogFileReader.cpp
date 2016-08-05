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
#include "LogFileReader.h"  // header for log file reader class
/***************************************************************************
 * This function is the default constructor for the log file reader class. *
 * The constructor takes in arguments the file name of the log file and    *
 * the default verbosity level associated with the file for displaying to  *
 * the log file viewer.                                                    *
 ***************************************************************************/
LogFileReader::LogFileReader(QString file_name, VerbosityLevel level)
{
  log_file_name = file_name;  // store the log file name 
  currentVLevel = level;  // store the default verbosity level
  output_text_document = NULL;  // no output text document initially
  num_bytes_read = 0;  // number of bytes read so far from log file is zero
  total_num_bytes = 0;  // total number of bytes in log file is zero
  num_lines_read = 0;  // number of lines of text read from file is zero
  num_lines_parsed = 0;  // number of lines of text parsed and outputted to viewer is zero
  cursor_ptr = NULL;  // cursor interface currently does not exist
}

/***************************************************************************
 * This function sets the text document that it will output text to.  The  *
 * text document is passed by the viewer into this log reader class so the *
 * reader class can use it to create a cursor and output text to it as the *
 * log file is being read and parsed.                                      *
 ***************************************************************************/
void LogFileReader::setTextDocument(QTextEdit *text_area, QTextDocument *text_doc)
{
  output_text_area = text_area;  // store the given text area in the class
  output_text_document = text_doc;  // store the given text document in the class
}

/***************************************************************************
 * This function creates a new cursor for manipulating text to display to  *
 * the viewer and 
 ***************************************************************************/
void LogFileReader::makeNewCursor()
{
  if(cursor_ptr!=NULL)  // if an old cursor exists
  {
    delete cursor_ptr;  // delete the old cursor
  }
  cursor_ptr = new QTextCursor(output_text_document);  // create cursor with the text document
  cursor_ptr->clearSelection();  // clear everything the new cursor is associated with
  cursor_ptr->movePosition(QTextCursor::Start, QTextCursor::MoveAnchor);  // cursor at start
}

/***************************************************************************
 * This function sets the verbosity level of the log file reader so it can *
 * display the proper text on the viewer associated with that verbosity    *
 * level.                                                                  *
 ***************************************************************************/
void LogFileReader::setVerbosityLevel(VerbosityLevel new_level)
{
  currentVLevel = new_level;  // set a new verbosity level for the file reader
}

/***************************************************************************
 * This function tells the log file reader to start reading the log file   *
 * that the class is associated with.  It calls the re ouptut function     *
 * that reads the data from the log file and parses it line by line and    *
 * displays them to the viewer through the text cursor interface.          *
 ***************************************************************************/
void LogFileReader::startRead()
{
  reOutputLog();  // calls function to read and output the log file text
}

/***************************************************************************
 * This function reads the log data from the log file and stores it in a   *
 * internal string list in memory.  It seeks to the position in the file   *
 * where the last byte was read and continues reading on.  That way when   *
 * new bytes are appended to the log files, they can be read very quickly. *
 ***************************************************************************/
void LogFileReader::readLogData()
{
  int num_new_bytes;       // number of new bytes in the log file since last read
  char read_buffer[1024];  // buffer used to hold latest data read from log files 
  bool failed_read;        // flag indicating whether read from file failed
  bool file_open_success;  // flag indicating whether opening the log file suceeded
  qint64 line_length;  // the length in bytes of the last line read from the log file

  log_file_ptr = new QFile(log_file_name);  // create a file object with the log file name
  
  /* open the log file as read only and in text/ascii format */
  file_open_success = log_file_ptr->open(QIODevice::ReadOnly | QIODevice::Text);

  if(file_open_success)  // if log file opened successfully
  {
    /* get the number of new bytes available for reading since last file read */
    num_new_bytes = log_file_ptr->bytesAvailable()-total_num_bytes;
    log_file_ptr->seek(total_num_bytes);  // move file pointer to location where new bytes were appended
    total_num_bytes += num_new_bytes;  // calculate the total number of new bytes in the log file
    failed_read = false;  // initially set the fail read flag to false 

   /***************************************************************************
    * Main loop that reads through the new data appended to the log file line *
    * by line and keeps track of how many lines of data read from the file.   *
    * The interesting aspect is that it will only insert the newly read data  *
    * into the internal string list if it is a complete line terminating with *
    * a new line character.  So if it reads a line and reaches the EOF but    *
    * the line contains no new line character, it will keep it in a temporary *
    * buffer and does not append to the internal string list.  New data will  *
    * continually be added to the temporary buffer until a new line character *
    * is read.  Then it will append the entire content of temporary buffer    *
    * into the string list.  This way, every string in the string list will   *
    * represent a complete line of text data.                                 *
    **************************************************************************/
    do  
    {
      line_length = log_file_ptr->readLine(read_buffer, 1024);  // read a line into the temporary buffer

      if (line_length!=-1)  // if reading from log file did not fail
      {
        if(line_length!=0)  // and if the number of bytes read is not zero
        {
          num_bytes_read += line_length;  // number of bytes read in last attempt accumulates into total number of bytes read
          temp_read_str_buffer.append(read_buffer);  // append the newly read bytes into a temporary string holder

          if(temp_read_str_buffer.contains(QChar('\n'), Qt::CaseSensitive)) // check to see if the last line read contains an newline character
          {
            log_file_text.append(temp_read_str_buffer);  // if contains new line then add it to the string list of already read lines
            temp_read_str_buffer.clear();  //  clear the temporary string buffer for the next read since it contains a new line and
            num_lines_read++;  // increment the total number of lines of text data read from the log file
          }
        }
      }
      else
      {
        failed_read = true;  // if reading from a log file failed then set the failed read flag
      }
    }
    while((total_num_bytes>num_bytes_read)&&(!failed_read));  // continue to read until all bytes are read or read failed

    log_file_ptr->close();  // close the file after reading complete
    delete log_file_ptr;  // delete the temporary file pointer
  }
}

/***************************************************************************
 * This function takes the content in the internal string list and outputs *
 * the text line by line into the text display window of the viewer. The   *
 * functions checks the number of lines already displayed in the viewer    *
 * and checks the total number of lines in the internal string list.  If   *
 * they don't match, then the new lines will be displayed into the viewer. *
 ***************************************************************************/
void LogFileReader::updateOutput()
{
  qint64 log_lines_size;  // the total number of lines of text in the internal string list
  QString temp_buffer;  // temporary string buffer holder
  QString long_verbosity_buffer;  // temporary string to hold the entire content of the log files

  log_lines_size = log_file_text.size(); // get the total number of lines of text in the internal list
  long_verbosity_buffer = "";

 /**************************************************************************
  * Conditional statement that checks the current verbosity level of the   *
  * reader and outputs the text based on the data verbosity level matching *
  * the current verbosity level of the reader.  The reader will output to  *
  * the viewer the verbosity level associated with the verbosity selection *
  * currently set by the user.                                             *
  **************************************************************************/
  if(currentVLevel==level_one)  // if verbosity level is one
  {
    /* loop through all of the lines until no more lines need to be parsed */
    while((num_lines_read>num_lines_parsed)&&(num_lines_parsed<log_lines_size))
    { 
      temp_buffer.clear();
      temp_buffer = log_file_text.value(num_lines_parsed);  // get the first new line that needs to be parsed

      if(temp_buffer.left(2)=="::")  // if first two characters are colons then they are verbosity level 1 text
      {
        temp_buffer.remove(0, 2);  // remove the colons
        long_verbosity_buffer += temp_buffer;
      }
      num_lines_parsed++;  // increment the number of lines parsed and outputted
    }
    outputNewText(long_verbosity_buffer);
  }
  else if(currentVLevel==level_two)  // if verbosity level is two
  {
    /* loop through all of the lines until no more lines need to be parsed */
    while((num_lines_read>num_lines_parsed)&&(num_lines_parsed<log_lines_size))
    {
      temp_buffer.clear();
      temp_buffer = log_file_text.value(num_lines_parsed);  // get the first new line that needs to be parsed

      if(temp_buffer.left(2)=="::")  // if first two colons then verbosity level 1
      {
        temp_buffer.remove(0, 2);  // remove the colons
        long_verbosity_buffer += temp_buffer;
      }
      else if(temp_buffer[0]==':')  // if first one colon then verbosity level 2
      {
        temp_buffer.remove(0, 1);  // remove the colons
        long_verbosity_buffer += temp_buffer;
      }
      num_lines_parsed++;  // increment the number of lines parsed and outputted
    }
    outputNewText(long_verbosity_buffer);
  }
  else if(currentVLevel==level_three)  // if verbosity level is three
  {
    /* loop through all of the lines until no more lines need to be parsed */
    while((num_lines_read>num_lines_parsed)&&(num_lines_parsed<log_lines_size))
    { 
      temp_buffer.clear();
      temp_buffer = log_file_text.value(num_lines_parsed);  // get the first new line that needs to be parsed

      if(temp_buffer.left(2)=="::")  // if first two colons then verbosity level 1
      { 
        temp_buffer.remove(0, 2);  // remove the colons
        long_verbosity_buffer += temp_buffer;
      }
      else if(temp_buffer[0]==':')  // if first one colon then verbosity level 2
      {
        temp_buffer.remove(0, 1);  // remove the colons
        long_verbosity_buffer += temp_buffer;
      }
      else
      {
        long_verbosity_buffer += temp_buffer;
      }
      num_lines_parsed++;  // increment the number of lines parsed and outputted
    }
    outputNewText(long_verbosity_buffer);
  }
  long_verbosity_buffer.clear();
}

/***************************************************************************
 * This function simply manipulates the text cursor interface so that the  *
 * text from the log file can be display to the viewer.  The function does *
 * outputting of text string from the parameter argument to the viewer     *
 * associated with the text cursor, which is associated with the text      *
 * document.  Uses cursor to draw to text document that outputs to viewer. *
 ***************************************************************************/
void LogFileReader::outputNewText(QString & str_buf)
{ 
  if(output_text_area!=NULL)  // if text interface exists for the current reader
  {
    output_text_area->insertPlainText(str_buf);  // insert the new text
  }
}

/***************************************************************************
 * This function clears the text in the viewer text document and then it   *
 * calls the output function to completely reparsed the text in memory and *
 * outputs it again to the viewer text document.  This function is very    *
 * useful if the verbosity level changed and thus, the text from the log   *
 * files have to be reanalyzed and outputted according to the verbosity    *
 * level.
 ***************************************************************************/
void LogFileReader::reOutputLog()
{
  num_lines_parsed = 0;  // total number of lines parsed for output reset

  if(output_text_area!=NULL)  // if a text document exists
  {
    output_text_document->clear();  // text document that contains the text of log file
    output_text_area->clear();  // clear the entire content of the text document
  }
  makeNewCursor();  // create a brand new cursor
  readLogData();  // read any new data from the file
  updateOutput();  // update the output to the viewer
}

/***************************************************************************
 * This function is used to change the verbosity level of the associated   *
 * reader so it can reoutput the text to the viewer with the new given     *
 * verbosity level.  Actually, this function calls reoutput function so    *
 * when the verbosity level changes through this function, it will update  *
 * the output to match the verbosity level automatically.                  *
 ***************************************************************************/
void LogFileReader::changeVerbosity(VerbosityLevel level)
{
  currentVLevel = level;  // set to the new verbosity level 
  reOutputLog();  // reoutput the content in the text document based on new verbosity level
}

/***************************************************************************
 * This function is the default destructor and it does garbage collection  *
 * of allocated memory such as the string list associated with the log     *
 * file, the cursor interface, and clears the viewer text document.        *
 ***************************************************************************/
LogFileReader::~LogFileReader()
{
  if(cursor_ptr!=NULL)  // if cursor interface exists
  {
    delete cursor_ptr;  // then delete the cursor interface
  }

  if(output_text_document!=NULL)  // if text document exists
  {
    output_text_document->clear();  // clear the document
  }
  log_file_text.clear();  // clear the internal string list
}
