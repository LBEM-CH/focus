#ifndef PATHBROWSER_H
#define PATHBROWSER_H

#include <QDir>
#include <QIcon>
#include <QString>
#include <QApplication>
#include <QDebug>
#include <QUrl>

class ApplicationData {
public:

    //Standard Directories
    static QDir applicationDir();
    static QDir homeDir();
    static QDir pluginsDir();
    static QDir translatorsDir();
    static QDir resourceDir();
    static QDir imagesDir();
    static QDir configDir();
    static QDir binDir();
    static QDir scriptsDir();
    static QDir procScriptsDir();
    static QDir kernelBinDir();
    
    //Extract icon with all it's states
    static QIcon icon(const QString& name);
    
    //Standard URLs
    static QUrl helpUrl();
    static QUrl bugReportUrl();
    
    //Standard inbuilt apps
    static QString logBrowserApp();
    static QString mergeApp();
    static QString viewerApp();
    static QString imageApp(); 
    static QString guiApp();
    static QString mrcConverterApp();
    
    //App icon Path
    static QString appIcon();
    
    //App Versions
    static QString versionNumber();
    static QString versionRevision();
    
    //Important Files
    static QString masterCfgFile();
    static QString userCfgFile();
    
    //Default publications
    static QStringList defaultPublicationsList();
        
};


#endif /* PATHBROWSER_H */

