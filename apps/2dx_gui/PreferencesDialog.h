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
#include <QButtonGroup>

#include "LineEditSet.h"

class QStackedWidget;
class QToolBar;

class PreferencesDialog : public QDialog {
    Q_OBJECT

public:
    PreferencesDialog(QWidget* parent = NULL);

private:
    QToolBar* setupToolBar();

    QWidget* getFontsPage();
    QWidget* getPathsPage();
    QWidget* getViewersPage();
    QWidget* getGeneralPage();
    
    void getToolButton(const QString& icon, const QString& text, int indexOfStackedWidget);
    
    QStackedWidget* pagesWidget_;
    QButtonGroup* toolBarButtonGroup_;
    QToolBar* contentsWidget_;
};

#endif 
