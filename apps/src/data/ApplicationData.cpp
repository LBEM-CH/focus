#include <sstream>

#include "Version.h"
#include "ApplicationData.h"

QDir ApplicationData::applicationDir() {
#ifdef Q_OS_MAC
    return QDir(QApplication::applicationDirPath() + "/../../../");
#else
    return QDir(QApplication::applicationDirPath());
#endif
}

QString ApplicationData::appIcon() {
    return applicationDir().absoluteFilePath("icons/icon.png");
}

QDir ApplicationData::binDir() {
    return QDir(applicationDir().absolutePath() + "/../bin/");
}

QUrl ApplicationData::bugReportUrl() {
    return QUrl("https://github.com/C-CINA/2dx/issues");
}

QDir ApplicationData::configDir() {
    return QDir(resourceDir().absolutePath() + "/config/");
}

QString ApplicationData::guiApp() {
    return binDir().absoluteFilePath("2dx_gui");
}

QString ApplicationData::mrcConverterApp() {
    return binDir().absoluteFilePath("2dx_mrc_converter");
}

QUrl ApplicationData::helpUrl() {
    return QUrl("http://2dx.org/documentation/2dx-software");
}

QDir ApplicationData::homeDir() {
    return QDir(QDir::homePath() + "/.2dx");
}

QIcon ApplicationData::icon(const QString& name) {
    QDir directory(imagesDir());
    if (!directory.exists()) {
        qDebug() << "CRITICAL! Icons directory does not exist!!";
        qDebug() << "Tried to locate:\n\t" << directory.absolutePath();
    }

    QIcon icon = QIcon();
    QString entry, type, label;

    foreach(entry, directory.entryList(QStringList() << name + QString("*"), QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
        if (entry.contains(QRegExp(".*\\-..\\.png$"))) {
            label = entry.section(".png", 0, 0).section("-", 0, 0).trimmed();
            type = entry.section(".png", 0, 0).section("-", 1, 1).trimmed().toLower();
            if (type == "ad" && label == name) icon.addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Active, QIcon::On);
            if (type == "id" && label == name) icon.addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Normal, QIcon::On);
            if (type == "au" && label == name) icon.addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Active, QIcon::Off);
            if (type == "iu" && label == name) icon.addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Normal, QIcon::Off);
        } else if (entry.contains(name + ".png", Qt::CaseInsensitive)) {
            icon.addPixmap(directory.canonicalPath() + "/" + entry);
        }
    }

    if (icon.isNull()) {
        qDebug() << "No icon present with requested name: " << name;
    }

    return icon;
}

QString ApplicationData::imageApp() {
    return binDir().absoluteFilePath("2dx_image");
}

QDir ApplicationData::imagesDir() {
    return QDir(resourceDir().absolutePath() + "/images/");
}

QDir ApplicationData::kernelBinDir() {
    return QDir(applicationDir().absolutePath() + "/../kernel/mrc/bin/");
}

QString ApplicationData::logBrowserApp() {
    return binDir().absoluteFilePath("2dx_logbrowser");
}

QString ApplicationData::mergeApp() {
    return binDir().absoluteFilePath("2dx_merge");
}

QString ApplicationData::viewerApp() {
    return binDir().absoluteFilePath("2dx_viewer");
}

QDir ApplicationData::pluginsDir() {
    return QDir(applicationDir().absolutePath() + "/../plugins/");
}

QDir ApplicationData::translatorsDir() {
    return QDir(pluginsDir().absolutePath() + "/translators/");
}

QDir ApplicationData::procScriptsDir() {
    return QDir(scriptsDir().absolutePath() + "/proc/");
}

QDir ApplicationData::resourceDir() {
    return QDir(applicationDir().absolutePath() + "/../resources/");
}

QDir ApplicationData::scriptsDir() {
    return QDir(applicationDir().absolutePath() + "/../scripts/");
}

QString ApplicationData::versionNumber() {
    // TODO introduce BOOST

    std::stringstream ss_major; //create a stringstream
    ss_major << VERSION_MAJOR; //add number to the stream
    std::string version_major = ss_major.str();

    std::stringstream ss_minor; //create a stringstream
    ss_minor << VERSION_MINOR; //add number to the stream
    std::string version_minor = ss_minor.str();

    std::stringstream ss_patch; //create a stringstream
    ss_patch << VERSION_PATCH; //add number to the stream
    std::string version_patch = ss_patch.str();

    std::string version_dot(".");

    std::string version_string = std::string();

    version_string += version_major;
    version_string += version_dot;
    version_string += version_minor;
    version_string += version_dot;
    version_string += version_patch;

    return QString::fromUtf8(version_string.c_str());
}

QString ApplicationData::versionRevision() {
    std::stringstream ss_major;     //create a stringstream
    ss_major << VERSION_REVISION;   //add number to the stream
    std::string revision = ss_major.str();

    return QString::fromUtf8(revision.c_str());
}

QString ApplicationData::masterCfgFile() {
    return configDir().absolutePath() + "/2dx_master.cfg";
}

QString ApplicationData::userCfgFile() {
    return configDir().absolutePath() + "/2dx.cfg";
}

QStringList ApplicationData::defaultPublicationsList() {
    QFile s(configDir().absolutePath() + "/publications.list");
    
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Publications list file read failed.";
        return QStringList();
    }

    QStringList publicationList;
    while (!s.atEnd()) publicationList << s.readLine().simplified();
    s.close();
    
    return publicationList;
}
