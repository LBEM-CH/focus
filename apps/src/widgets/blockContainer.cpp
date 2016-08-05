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
    setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    
    //setup title bar
    QWidget* header = setupHeader(title);
    
    //setup dummy main widget
    mainWidget = new QWidget(this);
    
    //Setup contents
    mainLayout = new QVBoxLayout(this);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    
    mainLayout->addWidget(header);
    mainLayout->addWidget(mainWidget);
    
    this->setLayout(mainLayout);
    
}

void blockContainer::setHeaderTitle(const QString& titleLabel)
{
    headerTitle->setText(titleLabel);
}

void blockContainer::setHeaderWidget(QWidget* widget)
{
    headerLayout->removeWidget(headerWidget);
    headerWidget = widget;
    headerLayout->addWidget(headerWidget, 0, 3, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
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
    
    //Set Height
    bar->setFixedHeight(20);
    
    //Set color of bar
    QPalette pal(bar->palette());
    QLinearGradient linearGradient(QPoint(0,0),QPoint(0,bar->height()));
    linearGradient.setColorAt(1.0, Qt::white);
    linearGradient.setColorAt(0.05, Qt::lightGray);
    linearGradient.setColorAt(0.0, Qt::gray);
    //pal.setBrush(QPalette::Window, linearGradient);
    //pal.setColor(QPalette::WindowText, Qt::darkGray);
    bar->setPalette(pal);
    
    //Setup Label
    headerTitle = new QLabel(title, this);
    headerTitle->setAutoFillBackground(false);
    headerTitle->setAlignment(Qt::AlignLeft);
    QPalette titlePal(headerTitle->palette());
    titlePal.setColor(QPalette::WindowText, Qt::darkGray);
    headerTitle->setPalette(titlePal);
    
    QFont font;
    font.setBold(true);
    //font.setStretch(QFont::SemiExpanded);
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
    headerLayout->addWidget(headerTitle, 0, 1, 1, 1, Qt::AlignLeft | Qt::AlignVCenter);
    headerLayout->addItem(spacer, 0, 2, 1, 1);
    headerLayout->addWidget(headerWidget, 0, 3, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
    
    return bar;
    
}

void blockContainer::mouseDoubleClickEvent(QMouseEvent *event)
{
	QWidget::mouseDoubleClickEvent(event);
	emit doubleClicked();
}