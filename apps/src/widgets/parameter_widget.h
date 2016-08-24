#ifndef CONFINTERFACE_H
#define CONFINTERFACE_H

#include <QWidget>
#include <QLabel>
#include <QHBoxLayout>
#include <QScrollArea>
#include <QToolBar>
#include <QToolButton>
#include <QString>
#include <QStringList>
#include <QMap>
#include <QLineEdit>

#include "graphicalButton.h"
#include "confData.h"
#include "parameter_section.h"

class ParametersWidget : public QWidget {
    Q_OBJECT

public:
    ParametersWidget(confData* data, QWidget *parent = NULL);
    ParametersWidget(confData* data, int userLevel, QWidget *parent = NULL);
    ParametersWidget(confData* data, QStringList parametersDisplayed, int userLevel = 0, QWidget *parent = NULL);
    
public slots:
    void setSelectionUserLevel(int);
    void changeParametersDisplayed(const QStringList& toBeDisplayed);
    void load();
    void resetParameters(const QMap<QString, QString>& toBeReset);
    void searchParams();

private:

    void initialize(int userLevel, const QStringList& toBeDisplayed);
    void changeFormWidget();

    QWidget* formWidget();
    QToolBar* toolWidget();

    QVBoxLayout* mainLayout_;
    QScrollArea* scrollArea_;
    QWidget* searchWidget;
    
    QLineEdit* searchBox;

    int userLevel_;
    QStringList parametersDisplayed_;
    QStringList parametersActuallyShown_;
    
    confData* data;
    
    QList<ParameterSection*> sections_;
};

#endif