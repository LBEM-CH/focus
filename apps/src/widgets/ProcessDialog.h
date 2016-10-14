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

class ProcessDialog : public QDialog {
    
public:
    ProcessDialog(QWidget* parent=0) : QDialog(parent) {
        
        setWindowTitle("2DX Process");
        setModal(true);
        
        titleLabel_ = new QLabel;
        
        progressBar_ = new QProgressBar;
        progressBar_->setMaximum(100);
        progressBar_->setFixedHeight(10);
        progressBar_->setValue(0);
        progressBar_->setTextVisible(false);
        
        statusWidget_ = new QListWidget;
        statusWidget_->setSelectionMode(QAbstractItemView::ExtendedSelection);
        statusWidget_->setAttribute(Qt::WA_MacShowFocusRect, 0);
        statusWidget_->hide();
    
        showMoreButton_ = new QPushButton("Show Details");
        showMoreButton_->setCheckable(true);
        connect(showMoreButton_, &QPushButton::toggled, [=](bool checked) {
            if(checked) {
                statusWidget_->show();
                showMoreButton_->setText("Hide Details");
            } else {
                statusWidget_->hide();
                showMoreButton_->setText("Show Details");
            }
        });
        
        QVBoxLayout* mainLayout = new QVBoxLayout;
        mainLayout->addStretch(0);
        mainLayout->addWidget(titleLabel_);
        mainLayout->addWidget(progressBar_);
        mainLayout->addWidget(showMoreButton_);
        mainLayout->addWidget(statusWidget_);
        
        setLayout(mainLayout);
        resize(400, 300);
    }
    
    void reset() {
        titleLabel_->setText("");
        progressBar_->reset();
        statusWidget_->clear();
    }
    
    void setLabelText(const QString& text) {
        titleLabel_->setText(text);
    }
    
    void addStatusText(const QString& text) {
        statusWidget_->addItem(text);
    }
    
    void setProgress(int value) {
        progressBar_->setValue(value);
    }
    
    void setRange(int min, int max) {
        progressBar_->setRange(min, max);
    }
    
    
private:
    QLabel* titleLabel_;
    QProgressBar* progressBar_;
    QPushButton* showMoreButton_;
    QListWidget* statusWidget_;
    
};

#endif /* PROCESSDIALOG_H */

