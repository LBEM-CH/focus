#include <QtWidgets>

#include "mrcImage.h"
#include "ApplicationData.h"
#include "Translator.h"

#include "ImageViewer.h"

ImageViewer::ImageViewer(const QString& workDir, const QString& notFoundMessage, QWidget* parent)
:QFrame(parent), notFoundMessage_(notFoundMessage), workingDir_(workDir){
    
    setAutoFillBackground(true);
    setBackgroundRole(QPalette::Base);
    setMinimumSize(QSize(235, 235));
    setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    
    imageLabel = new QLabel;
    imageLabel->setBackgroundRole(QPalette::Base);
    imageLabel->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    imageLabel->setAlignment(Qt::AlignCenter);
    //imageLabel->setScaledContents(true);
    
    mrcInfo = new MrcHeaderDisplay;

    widgets = new QStackedWidget;
    widgets->setMinimumSize(QSize(235, 235));
    widgets->addWidget(imageLabel);
    widgets->addWidget(mrcInfo);
    
    
    QGridLayout* layout = new QGridLayout;
    layout->setSpacing(0);
    layout->setMargin(0);
    layout->addWidget(widgets, 0, 0, Qt::AlignVCenter | Qt::AlignHCenter);
    setLayout(layout);
}

void ImageViewer::loadFile(const QString &fileName, bool loadInfo, const QString& notFoundMessage) {
    fileName_ = fileName;
    if (!notFoundMessage.isEmpty()) notFoundMessage_ = notFoundMessage;

    clearWidgets();
    
    if (!QFileInfo(fileName).exists()) {
        setText(notFoundMessage_);
        return;
    }

    if (loadInfo) {
        if (fileName.endsWith(".mrc")) {
            mrcHeader header(fileName);
            mrcInfo->setHeader(fileName, header);
            widgets->setCurrentWidget(mrcInfo);
        } else {
            setNotSupportedText();
        }
    } else {
        QImage image;
        if (fileName.endsWith(".mrc")) {
            mrcImage tempImage(fileName, true, this);
            image = *(tempImage.getImage());
        } else {
            image = QImage(fileName);
        }

        if (image.isNull()) {
            setNotSupportedText();
            return;
        }
        imageLabel->setPixmap(QPixmap::fromImage(image));
        imageLabel->adjustSize();
    }
}

void ImageViewer::setText(const QString& text) {
    QFont labelFont;
    labelFont.setPointSize(24);
    labelFont.setItalic(true);
    imageLabel->setText(text);
    imageLabel->setAlignment(Qt::AlignCenter);
    imageLabel->setFont(labelFont);
    imageLabel->adjustSize();
}

void ImageViewer::mouseDoubleClickEvent(QMouseEvent *event) {
    if(QFileInfo(fileName_).exists()) Translator(workingDir_, ApplicationData::translatorsDir().canonicalPath()).open(fileName_);
    QFrame::mouseDoubleClickEvent(event);
}

void ImageViewer::progressDialog() {
    QDialog *dialog = new QDialog(this);
    dialog->setModal(false);
    dialog->setFixedSize(QSize(700, 400));
    QGridLayout *dialogLayout = new QGridLayout(dialog);
    dialogLayout->setAlignment(Qt::AlignCenter);
    dialogLayout->addWidget(new QLabel("Generating Thumbnail", dialog));
    QProgressBar *progress = new QProgressBar(dialog);
    progress->setMaximum(100);
    progress->setValue(0);
    connect(this, SIGNAL(setProgress(int)), progress, SLOT(setValue(int)));
    dialogLayout->addWidget(progress);
    dialog->setLayout(dialogLayout);
    dialog->show();
}

void ImageViewer::clearWidgets() {
    imageLabel->clear();
    widgets->setCurrentWidget(imageLabel);
}

void ImageViewer::setNotSupportedText() {
    QString suffix = QFileInfo(fileName_).suffix().toUpper();
    setText(suffix + " file<br>(can't preview)");
}