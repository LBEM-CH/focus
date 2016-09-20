
#ifndef MRCHEADERDISPLAY_H
#define MRCHEADERDISPLAY_H

#include <QLabel>
#include <QGridLayout>

#include "mrcHeader.h"

class MrcHeaderDisplay : public QWidget {
    
public:
    MrcHeaderDisplay(QWidget *parent = NULL);
    void setHeader(const QString &result, mrcHeader header);
    
private:
    QString result;
    QList<QLabel *> titles;
    QList<QLabel *> labels;

    QLabel *titleLabel(const QString &text, bool color);
    void setColors(int start, int end, bool color);
};

#endif
