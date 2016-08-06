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

#include "graphicalButton.h"
#include "confData.h"
#include "confSectionHeader.h"

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

private:

    void initialize(int userLevel, const QStringList& toBeDisplayed);
    void changeFormWidget();

    QWidget* formWidget();
    QToolBar* toolWidget();

    QHBoxLayout* mainLayout_;
    QScrollArea* scrollArea_;

    int userLevel_;
    QStringList parametersDisplayed_;
    
    confData* data;
    
    QList<confSectionHeader*> sections_;
    QMap<QString, confElement*> parameterToElement_;
};

#endif