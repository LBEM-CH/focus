#include <iostream>

#include <QApplication>
#include <QDebug>
#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QFileInfo>
#include <QString>
#include <QMainWindow>
#include <QMessageBox>
#include <QFileInfo>
#include <QFile>

#include "ProjectData.h"
#include "ApplicationData.h"
#include "ImageNavigator.h"

using namespace std;

QStringList getAcceptableParamsList() {
    QFile listFile(ApplicationData::configDir().canonicalPath() + "/viewer.params.list");

    if (!listFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "The list of acceptable could not be read.";
        return QStringList();
    }

    QStringList paramsList;
    while (!listFile.atEnd()) paramsList << listFile.readLine().simplified().toLower();
    listFile.close();
    return paramsList;
}

void prepareWorkingDir(const QDir& workingDir) {
    QDir dir(workingDir);
    dir.mkdir("merge");
    dir.mkpath("merge/proc");
    dir.mkpath("merge/LOGS");
}

void prepareConfigFile(const QString& paramsFileName, const QString& workingDir) {
    QFile cfgFile(workingDir + "/merge/2dx_merge.cfg");
    if (!cfgFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QMessageBox::critical(NULL, "Could not write", "Was not able to write the file: " + cfgFile.fileName() + "\n\nWill quit now.");
        qApp->closeAllWindows();
    }

    QStringList paramsList = getAcceptableParamsList();
    if (QFileInfo(paramsFileName).exists()) {
        QFile paramsFile(paramsFileName);
        if (!paramsFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
            QMessageBox::critical(NULL, "File not read", "The parameters file provided: " + paramsFileName + " could not be read.\n");
        }

        while (!paramsFile.atEnd()) {
            QString line = paramsFile.readLine().trimmed();
            if (line.startsWith('#')) continue;

            if (line.startsWith("set")) line = line.remove(0, 3);

            if (line.contains('=')) {
                QStringList cell = line.split('=');
                if (paramsList.contains(cell.first().simplified().toLower())) {
                    cfgFile.write(QString("set " + cell.first().simplified() + " = " + '"' + cell.last().remove('"').simplified() + '"' + "\n").toLatin1());
                }
            }
        }

        paramsFile.close();
    }

    cfgFile.write("#\n#=============================================================================\n#\n", 83);
    cfgFile.close();
    QFile(workingDir + "/merge/" + "2dx_merge.cfg").link("merge/2dx_merge.cfg", workingDir + "/2dx_master.cfg");
}

void saveParams(const QString& parametersFile) {
    if (QFileInfo(parametersFile).exists()) {
        QFile paramsFile(parametersFile);
        if (!paramsFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
            QMessageBox::critical(NULL, "File not writable", "The parameters file provided: " + parametersFile + " could not be written.\n");
        }

        QStringList paramList = getAcceptableParamsList();
        ParametersConfiguration* conf = projectData.projectParameterData();
        for (int i = 0; i < paramList.size(); ++i) {
            if (conf->get(paramList[i])) {
                ParameterElementData* e = conf->get(paramList[i]);
                paramsFile.write(QString(e->name() + " = " + '"' + e->value().toString() + '"' + "\n").toLatin1());
            }
        }

        paramsFile.close();
    }
}

int main(int argc, char** argv) {
    QApplication app(argc, argv);
    qApp->setAttribute(Qt::AA_UseHighDpiPixmaps);
    QCoreApplication::setApplicationName("2DX Viewer (" + ApplicationData::versionNumber() + ")");
    QCoreApplication::setOrganizationName("C-CINA");
    QCoreApplication::setOrganizationDomain("c-cina.org");

    QCommandLineParser cliParser;
    cliParser.setApplicationDescription("2DX Viewer | 2DX Software Suite\nTo view and edit MRC files");
    cliParser.addHelpOption();
    cliParser.addVersionOption();
    cliParser.addPositionalArgument("image", "MRC type image to be opened");

    QCommandLineOption paramFileOption(QStringList() << "p" << "params", "A file containing the list of acceptable parameters", "params");
    cliParser.addOption(paramFileOption);

    QCommandLineOption workingDirOption(QStringList() << "w" << "dir", "Directory to put in temporary files", "workDir");
    cliParser.addOption(workingDirOption);

    cliParser.process(app);

    if (cliParser.positionalArguments().isEmpty()) {
        std::cout << cliParser.helpText().toStdString() << "\n";
        exit(0);
    }

    QString image = cliParser.positionalArguments().first();
    QString paramFile = cliParser.value(paramFileOption);
    QString workDir = cliParser.value(workingDirOption);

    if (workDir.isEmpty()) {
        workDir = QDir::tempPath() + "/2dx_viewer_tmp/";
        qDebug() << "The working dir is set to: " << workDir;
    }

    if (!QFileInfo(image).exists()) {
        QMessageBox::critical(NULL, "File not found", "The image file provided: " + image + " was not found.\n\nWill quit now.");
        qApp->closeAllWindows();
    }

    prepareWorkingDir(workDir);
    prepareConfigFile(paramFile, workDir);

    projectData.setParent(NULL);
    projectData.initiailze(QDir(workDir));

    ImageNavigator* navigator = new ImageNavigator(projectData.projectWorkingDir().canonicalPath(), new mrcImage(image), NULL);

    navigator->show();
    navigator->raise(); // raises the window on top of the parent widget stack
    navigator->activateWindow(); // activates the window an thereby putting it on top-level

    return app.exec();
}

