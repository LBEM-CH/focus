#ifndef BROWSER_WIDGET_HPP
#define BROWSER_WIDGET_HPP

#include <QWidget>
#include <QHBoxLayout>
#include <QLineEdit>
#include <QFileDialog>
#include <QString>
#include <QSize>
#include <QPushButton>

#include "ApplicationData.h"
#include "GraphicalButton.h"

class BrowserWidget : public QWidget {

    Q_OBJECT

public:

    enum class BrowseType {
        FILE,
        DIRECTORY
    };

    BrowserWidget(QWidget* parent)
    : QWidget(parent) {
        initialize();
    }

    BrowserWidget(BrowseType type = BrowseType::FILE, QString defaultPath = "", QWidget* parent = NULL)
    : QWidget(parent), type_(type), defaultPath_(defaultPath) {
        initialize();
    }

    QLineEdit* pathLineEditWidget() {
        return pathEdit_;
    }

    QString path() {
        return pathEdit_->text();
    }

    void setDefaultPath(const QString& path) {
        defaultPath_ = path;
    }

    void setType(const BrowseType type) {
        type_ = type;
    }

public slots:

    void browse() {
        QString path;

        if (type_ == BrowseType::DIRECTORY) path = QFileDialog::getExistingDirectory(this, tr("Select Directory"), defaultPath_);
        else path = QFileDialog::getOpenFileName(this, tr("Select File"), defaultPath_);

        if(!path.isEmpty()) {
            pathEdit_->setText(path);
            emit pathChanged(path);
        }
    }

    void setPath(const QString& path) {
        pathEdit_->setText(path);
        defaultPath_ = path;
    }

signals:
    void pathChanged(const QString&);

private:

    void initialize() {
        layout_ = new QHBoxLayout(this);
        layout_->setMargin(0);
        layout_->setSpacing(2);

        pathEdit_ = new QLineEdit(this);
        pathEdit_->setFrame(false);
        connect(pathEdit_, &QLineEdit::textEdited,
                [=](const QString& text) {emit pathChanged(text);});

        browseButton_ = new GraphicalButton(ApplicationData::icon("folder"), this);
        browseButton_->setFixedSize(QSize(pathEdit_->sizeHint().height(), pathEdit_->sizeHint().height()));
        connect(browseButton_, SIGNAL(clicked()), this, SLOT(browse()));

        layout_->addWidget(pathEdit_);
        layout_->addWidget(browseButton_, 0);

        setLayout(layout_);
    }

    GraphicalButton* browseButton_;
    QHBoxLayout* layout_;
    QLineEdit* pathEdit_;
    BrowseType type_;
    QString defaultPath_;
};

#endif

