#ifndef _STATUSVIEWER_H_
#define _STATUSVIEWER_H_

#include <QWidget>
#include <QDebug>
#include <QTimer>
#include <QLabel>
#include <QTableWidget>
#include <QHBoxLayout>

#include "FileWatcher.h"
#include "ProjectImage.h"

class StatusViewer : public QWidget {
    Q_OBJECT

public:
    StatusViewer(ProjectImage* image, QWidget *parent = NULL);

public slots:
    void load();
    void timedLoad();

private:

    QHBoxLayout* initializeLayout();
    void loadTable(QTableWidget* table, ParametersConfiguration* conf, const QStringList& rows, const QStringList& cols, const QString& sep, bool roundDigits = false);
    QTableWidget* prepareTable(int rows, int cols);
    
    QTableWidget* binsTable_;
    QTableWidget* qvalTable_;
    QTableWidget* tiltTable_;
    ProjectImage* image_;
    
    QTimer timer_;
    FileWatcher watcher_;
    
    QLabel* magLabel_;
    QLabel* qvalLabel_;
    QLabel* phaseResLabel_;
    QLabel* numSpotsLabel_;

};

#endif

