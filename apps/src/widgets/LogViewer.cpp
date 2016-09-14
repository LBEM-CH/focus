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
#include <QThread>
#include <QApplication>
#include <QAction>

#include "LogViewer.h"
#include <iostream>
using namespace std;

LogViewer::LogViewer(QString viewer_name, QAction *action, QWidget *parent) : QWidget(parent) {
    text_area_ptr = new QTextEdit(this); // create the text edit widget with scrolling capabilities
    text_area_ptr->setLineWrapMode(QTextEdit::NoWrap);
    text_area_ptr->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    text_area_ptr->setReadOnly(true); // set the text edit property to read only
    text_area_ptr->setAcceptRichText(true);
    if (action != NULL) text_area_ptr->addAction(action);
    text_area_ptr->setContextMenuPolicy(Qt::ActionsContextMenu);

    log_viewer_name = viewer_name; // store the viewer name
    num_bytes_read = 0; // initialize the number of bytes read from the file so far to be 0
    currentVLevel = level_one; // set the verbosity level to be 1

    text_document_ptr = text_area_ptr->document(); // get the pointer to the underlying text document
    text_cursor_ptr = new QTextCursor(text_document_ptr); // get the cursor to the text so can modify it
    text_document_ptr->setDefaultFont(QFont("Courier", 11));

    viewer_layout = new QGridLayout(this); // create a new grid layout for the viewer widget
    viewer_layout->setMargin(0);
    viewer_layout->setSpacing(0);

    viewer_layout->addWidget(text_area_ptr, 0, 0, 1, 1); // add the viewer widget to the layout
    setLayout(viewer_layout); // set the new layout
    //setMinimumWidth(7*80+30);
    connect(this, SIGNAL(fontInfoUpdated()), this, SLOT(updateFontInfo()));
    updateFontInfo();
}

void LogViewer::insertText(const QString &text) {
    if (text.startsWith("<<@Error>>")) {
        QString errorText = text;
        errorText.remove("<<@Error>>");
        text_area_ptr->insertHtml("<i><font color=\"#AD1211\">" + text + "</font></i>");
    } else {
        text_area_ptr->insertPlainText(text);
    }
    text_cursor_ptr->movePosition(QTextCursor::EndOfBlock);
    text_area_ptr->setTextCursor(*text_cursor_ptr);
    update();
}

void LogViewer::setStandardFont() {
    text_area_ptr->setTextColor(Qt::black);
    text_area_ptr->setFontItalic(false);
}

void LogViewer::setErrorFont() {
    text_area_ptr->setTextColor(QColor(173, 18, 17));
    text_area_ptr->setFontItalic(true);
}

void LogViewer::insertText(const QStringList &text) {
    text_cursor_ptr->movePosition(QTextCursor::EndOfBlock);
    text_area_ptr->setTextCursor(*text_cursor_ptr);

    QString textBlock, errorString, line;
    bool errorBegin = false, errorEnd = false, errorCurrent = false;
    setStandardFont();

    foreach(line, text) {
        errorBegin = line.contains(QRegExp("<\\s*error\\s*>"));
        errorEnd = line.contains(QRegExp("<\\s*/error\\s*>"));

        if (errorBegin) {
            errorCurrent = true;
        }
        if (errorEnd) {
            errorCurrent = false;
        }

        if (errorCurrent) errorString += line + "<br>";
        //insertError(line.remove(':').toAscii());
        //if(errorEnd) setStandardFont();

        if (errorBegin) line.remove(QRegExp("<\\s*error\\s*>"));
        if (errorEnd) line.remove(QRegExp("<\\s*/error\\s*>"));

        if (!errorCurrent) {
            if (line.startsWith("::")) {
                line.remove(0, 2);
                textBlock += line;
            } else if ((currentVLevel == level_two || currentVLevel == level_three) && line.startsWith(':')) {
                line.remove(0, 1);
                textBlock += line;
            } else if (currentVLevel == level_three) textBlock += line;
        }
    }
    text_area_ptr->insertPlainText(textBlock);

    text_cursor_ptr->movePosition(QTextCursor::EndOfBlock);
    text_area_ptr->setTextCursor(*text_cursor_ptr);

    if (!errorString.isEmpty()) insertError(errorString.toLatin1());

    update();
}

void LogViewer::insertError(const QByteArray &error) {

    text_cursor_ptr->movePosition(QTextCursor::EndOfBlock);
    text_area_ptr->setTextCursor(*text_cursor_ptr);

    text_area_ptr->insertHtml("<i><font color=\"#AD1211\">" + error + "<br></font></i>");

    text_cursor_ptr->movePosition(QTextCursor::EndOfBlock);
    text_area_ptr->setTextCursor(*text_cursor_ptr);
    update();
}

void LogViewer::updateFontInfo() {
    text_document_ptr->setDefaultFont(QFont("Courier", QApplication::font().pointSize() - 1));
}

void LogViewer::setVerbosityLevel(LogViewer::VerbosityLevel new_level) {
    currentVLevel = new_level;
}

void LogViewer::setVerbosity(int verbosity) {
    if (verbosity == 0 || verbosity == 1) currentVLevel = level_one;
    if (verbosity == 2) currentVLevel = level_two;
    if (verbosity == 3) currentVLevel = level_three;
}

void LogViewer::updateViewer() {
}

void LogViewer::clear() {
    text_document_ptr->clear();
    text_area_ptr->clear();
    update();
}

LogViewer::~LogViewer() {
}

QTextEdit* LogViewer::getTextEdit() {
    return text_area_ptr;
}

int LogViewer::currentVerbosity() {
    if (currentVLevel == level_one) return 1;
    if (currentVLevel == level_two) return 2;
    if (currentVLevel == level_three) return 3;
    return -1;
}

void LogViewer::setLogFile(const QString &logFileName) {
    logFile = logFileName;
}

const QString &LogViewer::getLogFile() {
    return logFile;
}

void LogViewer::loadLogFile(const QString &logFileName) {
    logFile = logFileName;
    load(currentVerbosity());
}

bool LogViewer::load(int verbosity) {
    setVerbosity(verbosity);
    clear();
    if (verbosity == 0) return true;
    if (logFile.isEmpty()) return false;
    QString line;
    QStringList text;
    QFile log(logFile);
    if (!log.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
    QTextStream stream(&log);
    if (verbosity == 3)
        stream.seek(QFileInfo(log).size() - 50024);
    while (!stream.atEnd()) {
        line = stream.readLine();
        text << line + '\n';
    }
    insertText(text);
    log.close();
    return true;
}


