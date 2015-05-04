/* 
 * File:   blockContainer.h
 * Author: biyanin
 *
 * Created on April 30, 2015, 3:56 PM
 */

#ifndef BLOCKCONTAINER_H
#define	BLOCKCONTAINER_H

#include <QWidget>
#include <QGridLayout>
#include <QString>
#include <QPalette>
#include <QColor>

class blockContainer : public QWidget
{
    Q_OBJECT
           
    public:
        blockContainer(QString title, QWidget* parent);
        
        void addWidget(QWidget* widget);
    
    private:
        QWidget* getTitleBar();
        
        QGridLayout* columnLayout;
        QString titleLabel;
        
        int columnCounter;
        
};

#endif	/* BLOCKCONTAINER_H */

