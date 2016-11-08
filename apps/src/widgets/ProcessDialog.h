#ifndef PROCESSDIALOG_H
#define PROCESSDIALOG_H

#include <QWidget>
#include <QDialog>
#include <QLabel>
#include <QProgressBar>
#include <QListWidget>
#include <QString>
#include <QStringList>
#include <QPushButton>
#include <QAbstractItemView>
#include <QVBoxLayout>

#define processDialog (ProcessDialog::Instance())

class ProcessDialog : public QDialog {
    
    Q_OBJECT
    
public:
    static ProcessDialog& Instance();
    
public slots:
    void reset();
    void setLabelText(const QString& text);
    void addStatusText(const QString& text);
    void setProgress(int value);
    void setRange(int min, int max);
    
private:
    
    ProcessDialog(QWidget* parent=0);
    
    QLabel* titleLabel_;
    QProgressBar* progressBar_;
    QPushButton* showMoreButton_;
    QListWidget* statusWidget_;
    
};

#endif /* PROCESSDIALOG_H */

