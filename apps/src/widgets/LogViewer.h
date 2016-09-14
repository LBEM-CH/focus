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

#ifndef LOGVIEWER_H
#define LOGVIEWER_H

#include <QWidget>
#include <QFile>
#include <QFileInfo>
#include <QTimer>
#include <QMenu>
#include <QAction>
#include <QGridLayout>
#include <QTextEdit>
#include <QTextDocument>
#include <QTextStream>
#include <QTextCursor>
#include <QList>
#include <QScrollBar>

class LogViewer : public QWidget {

    Q_OBJECT

public:

    enum VerbosityLevel {
        level_one, level_two, level_three
    };

    typedef struct {
        VerbosityLevel line_v_level;
        QString line_text;
        int num_bytes_read_in_text;
    } TextLine;

    typedef struct {
        int line_number;
        int byte_start_number;
        int byte_ending_number;
    } LineInfo;


    LogViewer(QString viewer_name, QAction *action, QWidget *parent = NULL); // constructor with viewer name and log file name
    ~LogViewer();
    
    void setVerbosityLevel(LogViewer::VerbosityLevel new_level); // change the verbosity level of the viewer
    void setLogFile(const QString &logFileName);
    const QString &getLogFile();
    QTextEdit *getTextEdit();

public slots:
    void updateViewer(); // updates the text in the viewer from the log files
    void clear();
    void insertText(const QString &text);
    void insertText(const QStringList &text);
    void insertError(const QByteArray &error);
    void updateFontInfo();
    bool load(int verbosity = 0);
    void setVerbosity(int verbosity);
    void loadLogFile(const QString &logFileName);

    void setStandardFont();
    void setErrorFont();

signals:
    void fontInfoUpdated();

private:

    int num_bytes_read;
    bool file_open_success;

    QString log_file_name;
    QString log_viewer_name;
    QFile *log_file_ptr;
    QTextStream *log_textstream_ptr;
    QTextEdit *text_area_ptr;
    QTextDocument *text_document_ptr;
    QTextCursor *text_cursor_ptr;
    QGridLayout *viewer_layout;

    QString logFile;

    VerbosityLevel currentVLevel;

    TextLine readLogLine();

    int loop;

    int currentVerbosity();

};

#endif
