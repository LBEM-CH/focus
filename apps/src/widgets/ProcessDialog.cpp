#include "ProcessDialog.h"

ProcessDialog::ProcessDialog(QWidget* parent) : QDialog(parent) {

    setWindowTitle("Focus Process");
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
    connect(showMoreButton_, &QPushButton::toggled, [ = ](bool checked){
        if (checked) {
            statusWidget_->show();
                    showMoreButton_->setText("Hide Details");
        } else {
            statusWidget_->hide();
                    showMoreButton_->setText("Show Details");
        }
    });

    QHBoxLayout* buttonsLayout = new QHBoxLayout;
    buttonsLayout->addWidget(showMoreButton_);
    buttonsLayout->addStretch(1);

    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->addWidget(titleLabel_, 0, 0);
    mainLayout->addWidget(progressBar_, 1, 0);
    mainLayout->addLayout(buttonsLayout, 2, 0);
    mainLayout->addWidget(statusWidget_, 3, 0);

    setLayout(mainLayout);
}

ProcessDialog& ProcessDialog::Instance() {
    static ProcessDialog instance_;
    return instance_;
}


void ProcessDialog::reset() {
    setWindowTitle("Focus Process");
    titleLabel_->setText("");
    progressBar_->reset();
    statusWidget_->clear();
    accept();
}

void ProcessDialog::addStatusText(const QString& text) {
    statusWidget_->addItem(text);
}

void ProcessDialog::setLabelText(const QString& text) {
    titleLabel_->setText(text);
}

void ProcessDialog::setProgress(int value) {
    progressBar_->setValue(value);
}

void ProcessDialog::setRange(int min, int max) {
    progressBar_->setRange(min, max);
}