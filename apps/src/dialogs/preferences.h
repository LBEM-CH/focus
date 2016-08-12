/* 
 * Author: Nikhil Biyani - nikhil(dot)biyani(at)gmail(dot)com
 *
 * This file is a part of 2dx.
 * 
 * 2dx is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or any 
 * later version.
 * 
 * 2dx is distributed in the hope that it will be useful, but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public 
 * License for more details <http://www.gnu.org/licenses/>.
 */

#ifndef PREFERENCES_HPP
#define PREFERENCES_HPP

#include <QObject>
#include <QDialog>
#include <QMap>

#include "confData.h"
#include "confElement.h"
#include "edit_set_widget.h"

class QStackedWidget;
class QToolBar;

class PreferencesDialog : public QDialog {
    Q_OBJECT

public:
    PreferencesDialog(confData* data, QWidget* parent = NULL);

public slots:
    void saveApps();
    void loadApps();

private:
    QToolBar* setupToolBar();

    QWidget* getApperancePage();
    QWidget* getAppsPage();

    QMap<confElement*, EditSetWidget*> elementToWidget;
    
    QStackedWidget* pagesWidget_;
    confData* mainData;
};

#endif 
