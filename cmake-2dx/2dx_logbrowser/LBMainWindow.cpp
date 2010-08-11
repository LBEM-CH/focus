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
#include "LBMainWindow.h"  // header for the main dialog box widget
/***************************************************************************
 *   This function is the default constructor for the main dialog box      *
 *   widget class.  It takes a command line argument as the directory      *
 *   where the log files are stored and creates a container that contains  *
 *   all the individual viewers for the log files.  It also initializes    *
 *   the widgets that are needed such as verbosity level and update button *
 *   for user interaction.  It also connects the widgets signals to their  *
 *   proper slots.                                                         *
 ***************************************************************************/
LBMainWindow::LBMainWindow(int argc, char *argv[], QWidget *parent) : QWidget(parent)
{
  desktop = new QDesktopWidget;  // get the desktop from the system
  working_screen = desktop->screen(desktop->primaryScreen());  // get the primary working screen from the system

  setMinimumWidth((working_screen->width())/3);  // make the minimum width of the browser window 1/3 of the width of the screen
  setMinimumHeight((working_screen->height())/3);  // make the minimum height of the browser window 1/3 of the height of the screen
  setBaseSize((working_screen->width())/2, (working_screen->height())/2);  // set the starting size of the window to be 1/2 of the screen 

  if(argc>1)  // directory is given where the log files are contained
  {
    if(argc>=3)  // if directory and startup log file are both given
    {
      viewers_ptr = new ViewerContainer(this, QString(argv[1]), QString(argv[2]), level_one); // create container and viewer with start file
    }
    else if(argc>=2)  // if only default directory is given
    {
      viewers_ptr = new ViewerContainer(this, QString(argv[1]), "", level_one);  // create container and viewers with no start file
    }
  }
  else  // no directory is given
  {
    viewers_ptr = new ViewerContainer(this, "./LOGS", "", level_one); // create container and viewers
  }

 /**************************************************************************
  * Initializing the verbosity level and update button widgets in the main *
  * dialog box and connecting them to the slot functions in the viewer.    *
  **************************************************************************/
  main_window_layout_ptr = new QGridLayout(this);  // grid style layout for the main dialog box
  update_button = new QPushButton("update", this);  // update  button for updating individual log viewers
  verb_buttons_group = new QButtonGroup(this);  // create button group that contains all verbosity level buttons
  verb_level_one_button = new QRadioButton("Verbosity Level 1", this);  // verbosity level 1 radio button
  verb_level_one_button->setChecked(true);  // verbosity level 1 as default so select the button on start up
  verb_level_two_button = new QRadioButton("Verbosity Level 2", this);  // verbosity level 2 radio button
  verb_level_three_button = new QRadioButton("Verbosity Level 3", this);  // verbosity level 3 radio button
  verb_buttons_group->addButton(verb_level_one_button, 1);  // add verbosity level 1 button to button group
  verb_buttons_group->addButton(verb_level_two_button, 2);  // add verbosity level 2 button to button group
  verb_buttons_group->addButton(verb_level_three_button, 3);  // add verbosity level 3 button to button group
  verb_buttons_group->setExclusive(true);  // verbosity level buttons exclusive so only one can be selected at a time
  main_window_layout_ptr->addWidget(verb_level_one_button, 0, 0, 1, 1);  // add verbosity level 1 button to main layout
  main_window_layout_ptr->addWidget(verb_level_two_button, 0, 1, 1, 1);  // add verbosity level 2 button to main layout
  main_window_layout_ptr->addWidget(verb_level_three_button, 0, 2, 1, 1);  // add verbosity level 3 button to main layout
  main_window_layout_ptr->addWidget(update_button, 0, 4, 1, 1);  // add update button to main layout
  main_window_layout_ptr->addWidget(viewers_ptr, 1, 0, 4, 5);  // add the viewer container with all viewers to main layout
  setLayout(main_window_layout_ptr);  // set the layout in the main dialog box

 /**************************************************************************
  * Connecting the proper signals from the widget to the proper functions  *
  * handling the signals generated by the user interface widget elements   *
  **************************************************************************/
  /* Connect changing verbosity level signal with handler that changes the output on the viewers */
  connect(verb_buttons_group, SIGNAL(buttonClicked(int)), viewers_ptr, SLOT(changeVerbosityLevel(int)));
  /* Connect update button signal with handler that changes that updates the contents of the viewers */
  connect(update_button, SIGNAL(clicked()), viewers_ptr, SLOT(updateLogViewers()));
}

/***************************************************************************
 *   This function is the default destructor for the main window class.    *
 ***************************************************************************/
LBMainWindow::~LBMainWindow()
{
  delete viewers_ptr;  // deletes the viewer container that contains all of the log viewers
                       // Qt will clean up the rest
}
