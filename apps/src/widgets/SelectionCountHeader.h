#ifndef IMAGELIBRARY_H
#define IMAGELIBRARY_H

#include <QWidget>
#include <QFrame>
#include <QGridLayout>
#include <QLabel>

class SelectionCountHeader : public QWidget { 
    Q_OBJECT
    
public:
    SelectionCountHeader(QWidget* parent=0);
    
public slots:
    void setHeaderTitle(const QString& title);

private:
    void setSelectionCount(int count);
    QWidget* setupHeader();
    
    QLabel* headerTitle;
    QLabel* selectionLabel;
};

#endif /* IMAGELIBRARY_H */

