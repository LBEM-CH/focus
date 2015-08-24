/* 
 * File:   blockContainer.h
 * Author: biyanin
 *
 * Created on April 30, 2015, 3:56 PM
 */

#include "blockContainer.h"

blockContainer::blockContainer(const QString& title, QWidget* parent)
            :QFrame(parent)
{
    setFrameStyle(QFrame::StyledPanel | QFrame::Sunken);
    
    //setup title bar
    QWidget* header = setupHeader(title);
    
    //setup dummy main widget
    mainWidget = new QWidget(this);
    
    //Setup contents
    mainLayout = new QVBoxLayout(this);
    this->setLayout(mainLayout);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    
    mainLayout->addWidget(header);
    mainLayout->addWidget(mainWidget);
    
}

void blockContainer::setHeaderTitle(const QString& titleLabel)
{
    headerTitle->setText(titleLabel);
}

void blockContainer::setHeaderWidget(QWidget* widget)
{
    headerLayout->removeWidget(headerWidget);
    headerWidget = widget;
    headerLayout->addWidget(headerWidget, 0, 3, 1, 1, Qt::AlignRight);
}

void blockContainer::setMainWidget(QWidget* widget)
{
    mainLayout->removeWidget(mainWidget);
    mainWidget = widget;
    mainLayout->addWidget(mainWidget);
}

QWidget* blockContainer::setupHeader(const QString& title)
{
    QWidget* bar = new QWidget(this);
    bar->setAutoFillBackground(true);
    
    //Set color of bar
    
    //Set Height
    bar->setFixedHeight(20);
    
    //Setup Label
    headerTitle = new QLabel(title, this);
    headerTitle->setAutoFillBackground(false);
    headerTitle->setAlignment(Qt::AlignLeft);
    
    QFont font;
    font.setBold(true);
    font.setStretch(QFont::SemiExpanded);
    headerTitle->setFont(font);
    
    //Setup header Widget
    headerWidget = new QWidget(bar);
    
    //Setup spacer
    QSpacerItem *spacer = new QSpacerItem(bar->width(),bar->height(),QSizePolicy::Maximum);
    
    //setup layout
    headerLayout = new QGridLayout(bar);
    headerLayout->setSpacing(0);
    headerLayout->setMargin(0);
    bar->setLayout(headerLayout);
    
    //Add widgets
    headerLayout->addItem(new QSpacerItem(3,3),0,0,1,1);
    headerLayout->addWidget(headerTitle, 0, 1, 1, 1, Qt::AlignLeft);
    headerLayout->addItem(spacer, 0, 2, 1, 1);
    headerLayout->addWidget(headerWidget, 0, 3, 1, 1, Qt::AlignRight);
    
    return bar;
    
}

void blockContainer::mouseDoubleClickEvent(QMouseEvent *event)
{
	QWidget::mouseDoubleClickEvent(event);
	emit doubleClicked();
}