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
