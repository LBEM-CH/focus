#ifndef UPDATEWINDOW_H
#define UPDATEWINDOW_H

#include <QWidget>
#include <QLabel>
#include <QPixmap>
#include <QGridLayout>
#include <QNetworkAccessManager>
#include <QUrl>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QDebug>
#include <QTextBrowser>
#include <QProcess>
#include <QPushButton>
#include <QMessageBox>
#include <QAction>

class UpdateWindow : public QWidget {
    Q_OBJECT
    
public:
    UpdateWindow(QWidget *parent = NULL);

public slots:
    void updateTextBox();
    void updateVersion();

private:

    void setWarningPalette(QWidget* widget);
    void setNormalPalette(QWidget* widget);
    
    QString getHtmlVersionInfo(QString changeText);

    QNetworkReply *updateInf;
    QTextBrowser *updateText;
    QLabel *updateTitle, *versionInfo, *revisionInfo;
    QPushButton *upgradeButton;
    QString installedVersion;

};

#endif

