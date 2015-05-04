/* 
 * File:   scrpitTab.cpp
 * Author: biyanin
 *
 * Created on April 30, 2015, 9:37 AM
 */

#include <iostream>
#include <levelGroup.h>
#include <viewContainer.h>

#include "scriptTab.h"


scriptTab::scriptTab(scriptModule* mod, confData* mainData, QWidget *parent)
    :QWidget(parent)
{
    module = mod;
    parameters = new confInterface(mainData,"");
    
    //Setup Parameters
    viewContainer* parameterContainer = new viewContainer("Setup (Standard)", viewContainer::data);

    localParameters = new resizeableStackedWidget(parameterContainer);

    QWidget *parametersWidget = new QWidget;
    QVBoxLayout *parameterLayout = new QVBoxLayout;
    parameterLayout->setMargin(0);
    parameterLayout->setSpacing(0);
    parametersWidget->setLayout(parameterLayout);
    parameterLayout->addWidget(localParameters);
    parameterLayout->addWidget(parameters);
    parameterLayout->setStretchFactor(localParameters,1);
    parameterLayout->setStretchFactor(parameters,50);
  
    parameterContainer->addScrollWidget(parametersWidget,true);

    levelGroup* userLevelButtons = new levelGroup(mainData,2,QStringList()<<"Setup (Standard)"<<"Setup (Advanced)",
                                                           QStringList()<<"Simplified Parameter List"<<"Full Parameter List",
                                                           QStringList()<<"gbAqua"<<"gbRed");
    parameterContainer->setHeaderWidget(userLevelButtons);
    connect(userLevelButtons,SIGNAL(titleChanged(const QString &)),parameterContainer,SLOT(setText(const QString &)));
    connect(userLevelButtons,SIGNAL(levelChanged(int)),parameters,SLOT(setSelectionUserLevel(int)));
    
    manual = new QStackedWidget;
    
    QGridLayout* layout = new QGridLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    this->setLayout(layout);
  
    module->setMaximumWidth(250);
    manual->setMaximumWidth(200);
    
    layout->addWidget(module, 0, 0, 1, 1);
    layout->addWidget(parameterContainer, 0, 1, 1, 1);
    layout->addWidget(manual, 0, 2 , 1, 1);
    
}

void scriptTab::loadParameters()
{
    parameters->load();
}


void scriptTab::setManualIndex(int index)
{
    manual->setCurrentIndex(index);
}

int scriptTab::addManualWidget(confManual* man)
{
    return manual->addWidget(man);
}

void scriptTab::hideManual()
{
    manual->hide();
}

void scriptTab::showManual()
{
    manual->show();
}
    
void scriptTab::setParameterIndex(int index)
{
    localParameters->setCurrentIndex(index - 1);
}

int scriptTab::addParameterWidget(confInterface* interf)
{
    return localParameters->addWidget(interf);
}

void scriptTab::hideLocalParameters()
{
    localParameters->hide();
}

void scriptTab::showLocalParameters()
{
    localParameters->show();
}

void scriptTab::selectPrameters(const QStringList &selectionList)
{
    parameters->select(selectionList);
}

void scriptTab::updateFontInfo()
{
    parameters->updateFontInfo();
}

scriptModule* scriptTab::getModule()
{
    return module;
}