#include <QtWidgets>

#include "mrcImage.h"
#include "ApplicationData.h"
#include "Translator.h"

#include "ImageViewer.h"

ImageViewer::ImageViewer(const QString& workDir, const QString& notFoundMessage, QWidget* parent)
:QFrame(parent), notFoundMessage_(notFoundMessage), workingDir_(workDir){
    
    setMinimumSize(QSize(200, 200));
    
    imageLabel = new QLabel;
    imageLabel->setBackgroundRole(QPalette::Base);
    imageLabel->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    imageLabel->setAlignment(Qt::AlignCenter);
    imageLabel->setScaledContents(true);
    
    mrcInfo = new MrcHeaderDisplay;
    fileInfo = new FileInfoDisplay;

    widgets = new QStackedWidget;
    widgets->setMinimumSize(QSize(200, 200));
    widgets->addWidget(imageLabel);
    widgets->addWidget(mrcInfo);
    widgets->addWidget(fileInfo);
    
    setFrameStyle(QFrame::StyledPanel | QFrame::Raised);
    
    QGridLayout* layout = new QGridLayout;
    layout->setSpacing(0);
    layout->setMargin(0);
    layout->addWidget(widgets, 0, 0, Qt::AlignVCenter | Qt::AlignHCenter);
    setLayout(layout);
}

void ImageViewer::setFileProperties(const QString& file, const QString& extension) {
    fileName_ = file;
    extension_ = extension.trimmed().toLower();
}

void ImageViewer::loadFile(const QString &fileName, const QString& extension, bool loadInfo) {
    fileName_ = fileName;
    extension_ = extension.trimmed().toLower();

    clearWidgets();
    
    if(fileName.isEmpty() || extension.isEmpty()) return;
    
    if (!QFileInfo(fileName).exists()) {
        setText(notFoundMessage_); 
        return;
    }
    
    if (loadInfo) {
        if (extension_ == "mrc" || extension_ == "map" || extension_ == "mrcs") {
            mrcHeader header(fileName);
            mrcInfo->setHeader(fileName, header);
            widgets->setCurrentWidget(mrcInfo);
        } else {
            fileInfo->setFile(fileName);
            widgets->setCurrentWidget(fileInfo);
        }
    } else {
        QImage image;
        if (extension_ == "mrc") {
            
            //Check if a png preview is available
            if(QFileInfo(fileName+".png").exists()) {
                if(QFileInfo(fileName).lastModified().toMSecsSinceEpoch() <= QFileInfo(fileName+".png").lastModified().toMSecsSinceEpoch()) {
                    image = QImage(fileName+".png");
                } else {
                    qDebug() << fileName << "had PNG, but is older, Time(MRC, PNG): " << QFileInfo(fileName).lastModified().toMSecsSinceEpoch() << QFileInfo(fileName+".png").lastModified().toMSecsSinceEpoch();
                    mrcImage tempImage(fileName);
                    image = *(tempImage.getImage());
                }
            } else {
                mrcImage tempImage(fileName);
                image = *(tempImage.getImage());
            }
        } else {
            image = QImage(fileName);
        }

        if (image.isNull()) {
            setNotSupportedText();
            return;
        }
        imageLabel->setPixmap(QPixmap::fromImage(image));
        resizeWidgets();
    }
}

void ImageViewer::setWorkDir(const QString& workDir) {
    workingDir_ = workDir;
}


void ImageViewer::setText(const QString& text) {
    QFont labelFont;
    labelFont.setPointSize(24);
    labelFont.setItalic(true);
    imageLabel->setText(text);
    imageLabel->setAlignment(Qt::AlignCenter);
    imageLabel->setFont(labelFont);
    resizeWidgets();
}

void ImageViewer::mouseDoubleClickEvent(QMouseEvent *event) {
    if(QFileInfo(fileName_).exists()) Translator(workingDir_, ApplicationData::translatorsDir().canonicalPath()).open(fileName_, extension_);
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
    setText(extension_.toUpper() + " file<br>(can't preview)");
}

void ImageViewer::resizeEvent(QResizeEvent* event) {
    QFrame::resizeEvent(event);
    resizeWidgets();
}

void ImageViewer::resizeWidgets() {
    if(imageLabel->pixmap()) {
        QSize newSize = imageLabel->pixmap()->size();
        newSize.scale(size().width(), size().height(), Qt::KeepAspectRatio);
        widgets->setFixedSize(newSize);
        imageLabel->setFixedSize(newSize);
    } else {
        widgets->setFixedSize(size());
        imageLabel->setFixedSize(size());
    }
    widgets->updateGeometry();
    imageLabel->updateGeometry();
    widgets->update();
    imageLabel->update();
}

