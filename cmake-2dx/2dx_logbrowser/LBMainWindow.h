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
#ifndef LBMAINWINDOW_H           // conditional compilation for protection
#define LBMAINWINDOW_H

#include <qdesktopwidget.h>      // qt desktop widget header class library
#include <qwidget.h>             // qt dialog box header class library
#include <qgridlayout.h>         // qt grid layout header class library
#include <qpushbutton.h>         // qt push button header library
#include <qbuttongroup.h>        // qt button group header library
#include <qradiobutton.h>        // qt radio button header library

#include "ViewerContainer.h"     // Container class header that contains the log viewers
#include "LogBrowserCommons.h"   // Common definitions for the project

class LBMainWindow;  // forward declaration
/***************************************************************************
 *   This class is the main window dialog box view for the log browser.    *
 *   It contains the viewer container class and the verbosity and update   *
 *   buttons.  It is the class for the primary widget for the application. *
 ***************************************************************************/
class LBMainWindow : public QWidget
{
    Q_OBJECT

    private:
    QGridLayout *main_window_layout_ptr;  // layout of the dialog box
    QPushButton *update_button;  // update button widget in the browser
    QDesktopWidget *desktop;  // the desktop widget that represents the working desktop
    QWidget *working_screen;  // the widget that represents the working desktop screen

    QButtonGroup *verb_buttons_group;  // button group that contains the radio buttons in the widget
    QRadioButton *verb_level_one_button;  // verbosity level 1 radio button
    QRadioButton *verb_level_two_button;  // verbosity level 2 radio button
    QRadioButton *verb_level_three_button;  // verbosity level 3 radio button
  
    ViewerContainer *viewers_ptr;  // container class that contains the actual viewers for each log file

    public:
    LBMainWindow(int argc, char *argv[], QWidget *parent=NULL);  // default constructor
    ~LBMainWindow();  // destructor
};

#endif
