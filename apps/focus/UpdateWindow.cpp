/**************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <QDesktopServices>
#include <QDebug>
#include <QFileInfo>

#include "UserPreferences.h"
#include "ApplicationData.h"
#include "BlockContainer.h"

#include "UpdateWindow.h"


UpdateWindow::UpdateWindow(QWidget *parent)
: QWidget(parent, Qt::Window) {

    QAction *hideWindow = new QAction(tr("Close Window"), this);
    hideWindow->setShortcut(tr("Ctrl+W"));
    addAction(hideWindow);
    connect(hideWindow, SIGNAL(triggered()), this, SLOT(hide()));

    setWindowTitle("Software Update");

    installedVersion = ApplicationData::versionNumber();

    setMinimumWidth(600);

    QVBoxLayout *layout = new QVBoxLayout(this);

    updateTitle = new QLabel("Focus Version Information");
    QFont font = updateTitle->font();
    font.setBold(true);
    font.setPointSize(18);
    updateTitle->setFont(font);

    versionInfo = new QLabel();
    versionInfo->setWordWrap(true);
    setNormalPalette(versionInfo);

    revisionInfo = new QLabel();
    revisionInfo->setWordWrap(true);
    setNormalPalette(revisionInfo);

    QVBoxLayout* headerLayout = new QVBoxLayout;
    headerLayout->addWidget(updateTitle);
    headerLayout->addWidget(versionInfo);
    headerLayout->addWidget(revisionInfo);
    headerLayout->addStretch(1);

    QHBoxLayout* topLayout = new QHBoxLayout;
    topLayout->addLayout(headerLayout);
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    topLayout->addWidget(spacer);

    if (QFileInfo(ApplicationData::appIcon()).exists()) {
        QLabel *appImage = new QLabel();
        appImage->setPixmap(QPixmap::fromImage(QImage(ApplicationData::appIcon())).scaledToHeight(50));
        appImage->setAlignment(Qt::AlignTop);
        topLayout->addWidget(appImage);
    } else {
        qDebug() << "Application logo not found";
    }

    layout->addLayout(topLayout, 0);


    QFrame* hLine = new QFrame(this);
    hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
    layout->addWidget(hLine);

    updateText = new QTextBrowser(this);
    BlockContainer* changesContainer = new BlockContainer("Release notes", updateText);
    layout->addWidget(changesContainer, 1);

    upgradeButton = new QPushButton("Update Version");
    connect(upgradeButton, SIGNAL(clicked()), this, SLOT(updateVersion()));
    layout->addWidget(upgradeButton, 0);

    QNetworkAccessManager* manager = new QNetworkAccessManager();
    QUrl url = QUrl("https://lbem-focus.epfl.ch/documentation.php");
    updateInf = manager->get(QNetworkRequest(url));
    connect(updateInf, SIGNAL(finished()), this, SLOT(updateTextBox()));

}

void UpdateWindow::updateTextBox() {
    if (updateInf->error() != QNetworkReply::NoError) {
        qDebug() << "Http error: " << updateInf->errorString();
        versionInfo->setText("No version information currently available.");
        updateText->insertPlainText("Http error: " + updateInf->errorString());
        return;
    }
    QByteArray updateHTML = updateInf->readAll();
    QStringList updateString = QString(updateHTML).split("######");
    QString currentFullVersion, currentReleaseText;
    
    if(updateString.size() > 1) currentFullVersion = updateString[1];
    if(updateString.size() > 2) currentReleaseText = updateString[2];
    
    QString currentVersion = currentFullVersion.split('-')[0];
    QString currentRevision;
    if (currentFullVersion.split('-').size() > 1) currentRevision = currentFullVersion.split('-')[1];
    QString remindUpdate = UserPreferences().getRemindUpdate().toLower();
    QString intVersion = currentVersion, intInstalled = installedVersion;
    intVersion.remove('.');
    intInstalled.remove('.');
    revisionInfo->setText("");
    upgradeButton->hide();
    if (intVersion.toInt() > intInstalled.toInt()) {
        versionInfo->setText("Version " + currentVersion + " is now available, your current version is " + installedVersion + ".<br>Would you like to upgrade?<br>");
        setWarningPalette(versionInfo);
        upgradeButton->show();
        if (remindUpdate != currentVersion) {
            int choice = QMessageBox::question(this, tr("New version available"), tr("A new version of Focus is available. <br> Would you like to upgrade?"), tr("Tell Me More"), tr("Skip this version"), tr("Not now"));
            if (choice == 0) show();
            if (choice == 1) UserPreferences().setRemindUpdate(currentVersion);
        }
    } else {
        versionInfo->setText("The currently installed version, " + installedVersion + " is the latest available.");
        setNormalPalette(versionInfo);

        if (!currentRevision.isEmpty()) {
            if (currentRevision != ApplicationData::versionRevision()) {
                revisionInfo->setText("Revision " + currentRevision + " is now available, your current revision is " +ApplicationData::versionRevision() + ".<br>Would you like to upgrade?<br>");
                setWarningPalette(revisionInfo);
                upgradeButton->show();
            } else {
                revisionInfo->setText("The currently installed revision, " + ApplicationData::versionRevision() + " is the latest available.");
                setNormalPalette(revisionInfo);
            }
        } else {
            revisionInfo->setText("No revision information available");
            setNormalPalette(revisionInfo);
        }
    }

    updateText->insertHtml(getHtmlVersionInfo(currentReleaseText));
    updateText->moveCursor(QTextCursor::Start);
}

QString UpdateWindow::getHtmlVersionInfo(QString releaseText) {
    QString releaseHtml = "<html><body>\n";
    QStringList releaseTextLines = releaseText.split("\n");
    QString version;
    QString state;
    QStringList added;
    QStringList fixed;
    bool startNew = true;
    for(QString line : releaseTextLines) {
        if(startNew) {
            version = "";
            state = "";
            added.clear();
            fixed.clear();
            startNew = false;
        }
        if(line.startsWith("VERSION:")) {
            version = line.remove("VERSION:").remove("\n");
        } else if(line.startsWith("STATE:")) {
            state = line.remove("STATE:").remove("\n");
        } else if(line.startsWith("ADDED:")) {
            added.append(line.remove("ADDED:").remove("\n"));
        } else if(line.startsWith("FIXED:")) {
            fixed.append(line.remove("FIXED:").remove("\n"));
        } else if(line.startsWith("---")) {
            releaseHtml += "<h2> Version: " + version + "</h2>";
            releaseHtml += "<p> Current state: " + state + "</p>";
            releaseHtml += "<h3> Improvements</h3><ul>";
            for(QString add : added) releaseHtml += "<li>" + add + "</li>";
            releaseHtml += "</ul> <h3> Bug Fixes</h3><ul>";
            for(QString fix : fixed) releaseHtml += "<li>" + fix + "</li>";
            releaseHtml += "</ul><br><hr><br>";
            startNew = true;
        }
    }
    
    releaseHtml += "</body></html>";
    return releaseHtml;
}

void UpdateWindow::updateVersion() {
    QDesktopServices::openUrl(QUrl("https://lbem-focus.epfl.ch/documentation.php"));
}

void UpdateWindow::setWarningPalette(QWidget* widget) {
    QPalette pal = widget->palette();
    pal.setColor(QPalette::WindowText, Qt::red);
    widget->setPalette(pal);
}

void UpdateWindow::setNormalPalette(QWidget* widget) {
    QPalette pal = widget->palette();
    pal.setColor(QPalette::WindowText, Qt::black);
    widget->setPalette(pal);
}
