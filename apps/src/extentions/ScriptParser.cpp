#include <QDebug>

#include "ProjectData.h"
#include "UserPreferenceData.h"
#include "ScriptParser.h"
#include "ResultsData.h"

ScriptParser::ScriptParser(const QDir& workDir, QObject* parent)
: QObject(parent), workingDir(workDir) {
}

int ScriptParser::parse(const QString &source, const QString &destination) {
    QFile s(source), d(destination);
    
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Template read failed.";
        return -1;
    }

    QStringList scriptData;
    QString line;

    while (!s.atEnd()) scriptData << s.readLine();
    s.close();

    if (scriptData[0].startsWith("#!/") && scriptData[0].contains("/csh")) {
        parseCsh(scriptData);
        executionCall = destination;
    }

    if (!d.open(QIODevice::WriteOnly | QIODevice::Text)) {
        qDebug() << "Script write failed for " << destination;
        return -2;
    }

    foreach(line, scriptData)
    d.write(line.toLatin1());

    d.setPermissions(QFile::ExeOwner | QFile::ReadOwner | QFile::WriteOwner);
    d.close();

    return 0;
}

const QString &ScriptParser::executionString() {
    return executionCall;
}

void ScriptParser::execute(const QString &script) {
    QProcess process;
    process.setWorkingDirectory(workingDir.canonicalPath());
    process.setStandardOutputFile(workingDir.canonicalPath() + "/LOGS/" + script + ".log");
    process.start(executionCall, QIODevice::ReadOnly);
    process.waitForFinished(6 * (60 * 60 * 1000));
    ResultsData resultsData(workingDir);
    resultsData.load(workingDir.canonicalPath() + "/LOGS/" + script + ".results");
    resultsData.save();
}

bool ScriptParser::parseCsh(QStringList& scriptData) {
    return parseCsh(workingDir, scriptData);
}

bool ScriptParser::parseCsh(const QDir& workingDir, QStringList& scriptData) {
    int i = 0;
    QString line;
    QStringList vSearch;
    vSearch << "sortorder" << "remark" << "display" << "global" << "section" << "===" << "manual" << "reset";

    while (!line.contains("$end_local_vars", Qt::CaseInsensitive) && i < scriptData.size()) {
        line = scriptData[i].trimmed().toLower();
        line = line.remove('#').trimmed();

        if (line.startsWith("set ")) {
            int k = scriptData[i].indexOf('=');
            if (k > 0) {
                QStringList cell = scriptData[i].split('=');
                cell[0].remove(0, 4);
                if (cell[0].trimmed() == "bin_2dx") {
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::kernelBinDir().absolutePath() + '"');
                } else if (cell[0].trimmed() == "proc_2dx")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::procScriptsDir().absolutePath() + '"');
                else if (cell[0].trimmed() == "app_2dx_mrc_converter")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::mrcConverterApp() + '"');
                else if (cell[0].trimmed() == "app_2dx_viewer")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::viewerApp() + '"');
                else {
                    ParametersConfiguration* data = projectData.parameterData(workingDir);
                    if(data) {
                        QString v = data->getValue(cell[0].trimmed());
                        scriptData[i].replace(QRegExp("\".*\""), '"' + v + '"');
                        scriptData.insert(++i, QString("echo \"") + cell[0].trimmed() + " = " + v + "\"\n");
                    }
                    else {
                        qDebug() << "Parameter config not found for: " << workingDir;
                    }
                }

            }
        }

        for (int k = 0; k < vSearch.size(); k++)
            if (line.startsWith(vSearch[k].toLower())) {
                scriptData.removeAt(i);
                i--;
                break;
            }


        i++;
    }

    if (i > 0 && scriptData[i - 1].toLower().contains("$end_local_vars")) scriptData.removeAt(i - 1);

    scriptData.insert(++i, "#\n");
    scriptData.insert(++i, "echo \" \"\n");
    scriptData.insert(++i, "echo \"############################################\"\n");
    scriptData.insert(++i, "echo \"# Parameters requested by the Script       #\"\n");
    scriptData.insert(++i, "echo \"############################################\"\n");
    scriptData.insert(++i, "#\n");

    while (!line.contains("$end_vars") && i < scriptData.size()) {
        line = scriptData[i].trimmed().toLower();

        if (line.contains("set ")) {
            int k = line.indexOf('=');
            if (k > 0) {
                QStringList cell = line.split('=');
                cell[0].remove(0, 4);
                if (cell[0].trimmed() == "bin_2dx") {
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::kernelBinDir().absolutePath() + '"');
                } else if (cell[0].trimmed() == "proc_2dx")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::procScriptsDir().absolutePath() + '"');
                else if (cell[0].trimmed() == "app_2dx_mrc_converter")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::mrcConverterApp() + '"');
                else if (cell[0].trimmed() == "app_2dx_viewer")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::viewerApp() + '"');
                else {
                    ParametersConfiguration* data = projectData.parameterData(workingDir);
                    if(data) {
                        QString v = data->getValue(cell[0].trimmed());
                        scriptData[i].replace(QRegExp("\".*\""), '"' + v + '"');
                        scriptData.insert(++i, QString("echo \"") + cell[0].trimmed() + " = " + v + "\"\n");
                    }
                    else {
                        qDebug() << "Parameter config not found for: " << workingDir;
                    }
                }
            }
        }
        i++;
    }

    if (i < scriptData.size()) scriptData.removeAt(i - 1);

    //Add the paths
    scriptData.insert(i++, "#\n");
    scriptData.insert(i++, "echo \" \"\n");
    scriptData.insert(i++, "echo \"############################################\"\n");
    scriptData.insert(i++, "echo \"# Parameters from the Preferences settings #\"\n");
    scriptData.insert(i++, "echo \"############################################\"\n");
    scriptData.insert(i++, "#\n");
    for (unsigned int ii = 0; ii < userPreferenceData.data()->size(); ii++) {
        if ((*userPreferenceData.data())[ii]->title().trimmed().toLower().contains("software")
                || (*userPreferenceData.data())[ii]->title().trimmed().toLower().contains("microscope") 
                || (*userPreferenceData.data())[ii]->title().trimmed().toLower().contains("system")
                || (*userPreferenceData.data())[ii]->title().trimmed().toLower().contains("status")) {
            ParameterElementData *element;
            for (unsigned int j = 0; j < (*userPreferenceData.data())[ii]->size(); j++) {
                element = (*(*userPreferenceData.data())[ii])[j];
                scriptData.insert(i++, "set " + element->name() + " = " + '"' + element->value().toString() + '"' + "\n");
                scriptData.insert(i++, QString("echo \"") + element->name() + " = " + element->value().toString() + "\"\n");
            }
        }
    }
    scriptData.insert(i++, "#\n");
    scriptData.insert(i++, "echo \" \"\n");
    scriptData.insert(i++, "echo \"#############################################\"\n");
    scriptData.insert(i++, "echo \"# Now the remainder of the original script: #\"\n");
    scriptData.insert(i++, "echo \"#############################################\"\n");
    scriptData.insert(i++, "echo \" \"\n");
    scriptData.insert(i++, "#\n");
    
    return true;
}

