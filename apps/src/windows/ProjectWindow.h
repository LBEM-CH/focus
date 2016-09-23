#ifndef PROJECTWINDOW_H
#define PROJECTWINDOW_H

#include <QWidget>

#include "ExecutionWindow.h"

class ProjectWindow : public QWidget {
    
    Q_OBJECT

public:
    ProjectWindow(QWidget* parent) 
    : QWidget(parent){
    };
    
    
    
private:
    
    QLabel* projectNameLabel_;
    QLabel* projectPathLabel_;
    QLabel* importCountLabel_;
    
    ExecutionWindow* exeWindow_;
    
};


#endif /* PROJECTWINDOW_H */

