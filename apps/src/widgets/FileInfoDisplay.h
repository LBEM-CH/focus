
#ifndef FILEINFODISPLAY_H
#define FILEINFODISPLAY_H

#include <QLabel>
#include <QGridLayout>

#include "mrcHeader.h"

class FileInfoDisplay : public QWidget {
    
public:
    FileInfoDisplay(QWidget *parent = NULL);
    void setFile(const QString &result);
    
private:
    QString result;
    QList<QLabel *> titles;
    QList<QLabel *> labels;

    QLabel *titleLabel(const QString &text, bool color);
    void setColors(int start, int end, bool color);
};

#endif
