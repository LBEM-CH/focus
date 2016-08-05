#include <QApplication>
#include <QDir>
#include <QDebug>
#include <QProcess>
#include <QFileInfo>
#include <QCommandLineParser>

#include "confData.h"
#include "scriptParser.h"

QString getAppDir() {
    QString appDir = QApplication::applicationDirPath();
    QString sep = "/../";
#ifdef Q_OS_MAC
    appDir += "/../../../";
#endif
    int tries = 0;
    while (!QFileInfo(appDir + sep + "config/2dx_master.cfg").exists() && tries < 3) {
        qDebug() << (appDir + sep + "config/2dx_master.cfg") << " does not exist!";
        sep += "../";
        tries++;
    }
    if (QFileInfo(appDir + sep + "config/2dx_master.cfg").exists()) {
        return QString(appDir + sep);
    } else
        return QString();

}

bool backupSelection(const QString selectionFileName) {
    std::cout << "backing up " << selectionFileName.toStdString() << std::endl;
    bool status = QFile::rename(selectionFileName, selectionFileName + ".bak");
    if (!status)
        std::cerr << "moving " << selectionFileName.toStdString() << " to " << selectionFileName.toStdString() << ".bak failed \n";
    return status;
}

bool restoreSelection(const QString selectionFileName) {
    return QFile::rename(selectionFileName + ".bak", selectionFileName);
}

bool backupDefaultSelection(QDir mergeDir) {
    QString defaultSelection(mergeDir.absolutePath() + "/2dx_merge_dirfile.dat");
    if (!QFileInfo(defaultSelection).exists())
        std::cerr << defaultSelection.toStdString() << " does not exist.\n";
    return backupSelection(defaultSelection);
}

bool restoreDefaultSelection(QDir mergeDir) {
    QString defaultSelection(mergeDir.absolutePath() + "/2dx_merge_dirfile.dat");
    bool status = QFile::remove(defaultSelection);
    return restoreSelection(defaultSelection) && status;
}

bool writeSelection2File(QList<QString> selection, const QString fileName, QString projectPath) {
    std::cout << "project-path " << projectPath.toStdString() << std::endl;
    QFile file(fileName);
    if (file.open(QFile::WriteOnly | QFile::Text)) {
        QTextStream s(&file);
        for (int i = 0; i < selection.size(); ++i) {
            QString image = selection.at(i);
            if (QFileInfo(projectPath + image).exists())
                s << image << '\n';
            else {
                std::cerr << "file " << image.toStdString() << " does not exist." << std::endl;
                return false;
            }
        }
    } else {
        std::cerr << "error opening selection file\n";
        return false;
    }
    file.close();
    return true;
}

bool setSelection(QList<QString> selection, QDir mergeDir) {
    //QFile selectionFile;
    QString defaultSelection(mergeDir.absolutePath() + "/2dx_merge_dirfile.dat");
    if (QFileInfo(defaultSelection).exists()) {
        if (!backupDefaultSelection(mergeDir)) {
            std::cerr << "error backing up selection " << defaultSelection.toStdString() << std::endl;
            return false;
        }
    }

    QString selectionString = selection[0];
    selectionString = selectionString.remove('"').trimmed();
    if (selectionString.endsWith(".dat")) {
        QString selectionPath(mergeDir.absolutePath() + "/" + selectionString);
        if (QFileInfo(defaultSelection).exists())
            std::cerr << "default selection does still exist: " << defaultSelection.toStdString() << std::endl;

        if (QFileInfo(selectionPath).exists()) {
            bool status = QFile::rename(selectionPath, defaultSelection);
            //TODO: check the status, but right now it seems to retunr false, even when the rename works
            if (!status) {
                std::cerr << "error moving " << selectionPath.toStdString() << " to " << defaultSelection.toStdString() << std::endl;
                return false;
            }
        } else {
            std::cerr << "specified selection file " << selectionPath.toStdString() << " does not exist." << std::endl;
            return false;
        }
    } else {
        QString projectPath = mergeDir.absoluteFilePath("../");
        return writeSelection2File(selection, defaultSelection, projectPath);
    }
    return true;
}

bool restoreSelections(QList<QString> selection, QDir mergeDir) {
    QString defaultSelection(mergeDir.absolutePath() + "/2dx_merge_dirfile.dat");
    QString selectionString = selection[0];
    selectionString = selectionString.remove('"').trimmed();
    if (selectionString.endsWith(".dat")) {
        QString specSelection(mergeDir.absolutePath() + "/" + selectionString);
        if (!QFile::rename(defaultSelection, specSelection)) {
            std::cerr << "error moving " << defaultSelection.toStdString() << " to " << specSelection.toStdString() << std::endl;
            return false;
        }
    }
    return restoreDefaultSelection(mergeDir);
}

confData* readConfig(const QString workingPath, const QString applicationPath) {
    confData* data;
    QString mergeConfigLocation = workingPath + "/merge/" + "2dx_merge.cfg";
    QString appConfigLocation = applicationPath + "config" + "/" + "2dx_master.cfg";
    if (!QFileInfo(mergeConfigLocation).exists()) {
        std::cerr << "Config file " + mergeConfigLocation.toStdString() << " does not exist!";
        std::cerr << "Please initialize the project via the graphical user interface of 2dx_merge.";
        return 0;
    }
    data = new confData(mergeConfigLocation, appConfigLocation);
    std::cout << "set config defaults." << std::endl;
    data->setDefaults(workingPath);
    if (QFileInfo(appConfigLocation).exists()) {
        data->updateConf(appConfigLocation);
    }

    data->setDir("project", QDir(workingPath));
    data->setDir("working", QDir(workingPath + "/merge"));
    data->save();
    return data;
}

bool scriptExists(QString scriptName, QDir scriptsDir) {
    QStringList scriptList = scriptsDir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted);
    return scriptList.contains(scriptName, Qt::CaseInsensitive);
}

void execute(QString scriptName, QString scriptsDir, confData* config) {
    confData* scriptConf;
    QString scriptPath = scriptsDir + "/" + scriptName;
    scriptName.remove(QRegExp("\\.script$"));
    if (QFileInfo(scriptPath).exists()) {
        scriptConf = new confData(scriptPath);
    }
    scriptParser parser(QList<confData *>() << scriptConf << config);
    std::cout << "::  Executing in " << config->getDir("working").toStdString() << " : " << scriptName.toStdString() << std::endl;
    parser.parse(scriptPath, config->getDir("working") + "/proc/" + scriptName + ".com");
    parser.execute(scriptName, config);

    if (scriptConf) delete scriptConf;
}

void commandLineMerge(const QString appDir, const QString workingDir, const QStringList fileSelection, const QString script) {
    confData* config;
    config = readConfig(workingDir, appDir);
    QDir mergeDir = config->getDir("working");
    if (!setSelection(fileSelection, mergeDir)) {
        restoreDefaultSelection(mergeDir);
        return;
    }

    QString merge2DScripts = appDir + "/../kernel/2dx_merge/scripts-merge2D/";
    QString merge3DScripts = appDir + "/../kernel/2dx_merge/scripts-merge3D/";
    QString customScripts = appDir + "/../kernel/2dx_merge/scripts-custom/";
    QString spScripts = appDir + "/../kernel/2dx_merge/scripts-singleparticle/";
    
    QString strippedScript = QString(script).remove('"').trimmed();
    QString scriptFile = strippedScript + ".script";
    std::cout << "script file: " << scriptFile.toStdString() << std::endl;
    
    QDir scriptsDir;
    if(QFileInfo(merge2DScripts + scriptFile).exists()) scriptsDir = QDir(merge2DScripts);
    else if(QFileInfo(merge3DScripts + scriptFile).exists()) scriptsDir = QDir(merge3DScripts);
    else if(QFileInfo(customScripts + scriptFile).exists()) scriptsDir = QDir(customScripts);
    else if(QFileInfo(spScripts + scriptFile).exists()) scriptsDir = QDir(spScripts);
    else {
        std::cerr << "\n\nERROR: There is no script " << scriptFile.toStdString() << std::endl;
        return;
    }
    
    execute(scriptFile, scriptsDir.absolutePath(), config);   
    restoreSelections(fileSelection, mergeDir);
    delete config;
}

int main(int argc, char **argv) {

    QCoreApplication app(argc, argv);
    QCoreApplication::setApplicationName("2dx_merge");
    QCoreApplication::setOrganizationName("C-CINA");
    QCoreApplication::setOrganizationDomain("c-cina.org");

    QCommandLineParser cliParser;
    cliParser.setApplicationDescription("2DX Software Suite: 2dx_merge Command Line Version\n(If you were looking for GUI, try 2dx_gui instead.)");
    cliParser.addHelpOption();
    cliParser.addVersionOption();
    cliParser.addPositionalArgument("project_dir", "Project directory path.");
    cliParser.addPositionalArgument("script", "Script to be executed");
    cliParser.addPositionalArgument("selection_files", "File(s) with the selection of images");
    cliParser.process(app);

    QStringList arguments = cliParser.positionalArguments();
    if (arguments.size() >=2 ) {
        
        QString applicationDir = getAppDir();
        std::cout << "Application Directory: " << applicationDir.toStdString() << std::endl;
        
        QString workingDir = arguments[0];
        std::cout << "Project Directory: " << workingDir.toStdString() << std::endl;
        
        QString script = arguments[1];
        std::cout << "Script to run: " << script.toStdString() << std::endl;
        
        QList<QString> fileSelection;
        if (arguments.size() > 2) {
            for (int i = 0; i < 2; ++i) arguments.pop_front();
            fileSelection = arguments;
        } else {
            std::cout << "Warning! No selection specified." << std::endl;
            std::cout << "  taking 2dx_merge_dirfile.dat" << std::endl;
            fileSelection << "2dx_merge_dirfile.dat";
        }
        commandLineMerge(applicationDir, workingDir, fileSelection, script);
    } else {
        std::cout << cliParser.helpText().toStdString() << "\n";
        exit(0);
    }
}
