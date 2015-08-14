/* 
 * File:   blockContainer.h
 * Author: biyanin
 *
 * Created on April 30, 2015, 3:56 PM
 */

#include <QLinearGradient>
#include <QPoint>
#include <QFont>
#include <QGridLayout>
#include <QLabel>

#include "blockContainer.h"

blockContainer::blockContainer(QString title, QWidget* parent)
            :QWidget(parent)
{
    columnCounter=0;
    titleLabel = title;
    
    //Setup size
    this->setMinimumHeight(250);
    
    //Setup contents
    QGridLayout* mainLayout = new QGridLayout(this);
    this->setLayout(mainLayout);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    
    //Add title bar
    //mainLayout->addWidget(getTitleBar(), 0, 0, 1, 1);
    
    //Add widget Container
    QWidget* widgetContainer = new QWidget(this);
    columnLayout = new QGridLayout(widgetContainer);
    columnLayout->setMargin(2);
    columnLayout->setSpacing(2);
    widgetContainer->setLayout(columnLayout);
    
    mainLayout->addWidget(widgetContainer, 0, 0, 1, 1);
    
    
}


QWidget* blockContainer::getTitleBar()
{
    QWidget* bar = new QWidget(this);
    bar->setAutoFillBackground(true);
    
    //Set Height
    bar->setFixedHeight(20);
    
    //Set Font
    QFont font;
    font.setBold(true);
    font.setPixelSize(13);
    font.setStretch(QFont::SemiExpanded);
    
    //Set Text
    QLabel* titleText = new QLabel(titleLabel, this);
    
    titleText->setFont(font);
    titleText->setAutoFillBackground(false);
    titleText->setAlignment(Qt::AlignCenter);
    
    QGridLayout* layout = new QGridLayout(bar);
    layout->setSpacing(0);
    layout->setMargin(0);
    layout->addWidget(titleText,0,1,1,1);
    layout->setAlignment(titleText,Qt::AlignCenter);
    bar->setLayout(layout);
    
    return bar;
    
}

void blockContainer::addWidget(QWidget* widget)
{
    columnLayout->addWidget(widget, 0, columnCounter, 1, 1);
    columnCounter++;
}

