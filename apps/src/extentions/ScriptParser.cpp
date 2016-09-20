#include <QDebug>

#include "ProjectData.h"
#include "UserPreferenceData.h"
#include "ScriptParser.h"

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

bool ScriptParser::parseResults(ParametersConfiguration* conf, const QString &results) {
    //  currentResults = results;
    QFile data(results);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) return 0;

    QString line;
    while (!data.atEnd()) {
        line = data.readLine();
        QRegExp lock("^\\s*#\\s(un)?lock\\s(.*)$", Qt::CaseInsensitive);
        QRegExp var("^\\s*set\\s*(\\S+)\\s*=\\s*\\\"{0,1}(.*)\\\"{0,1}");
        if (var.indexIn(line) != -1) {
            QString variable = var.cap(1).trimmed();
            QString value = var.cap(2).trimmed().remove('"');
            if (!variable.isEmpty() && !value.isEmpty())
                conf->set(variable, value);
        }
        if (lock.indexIn(line) != -1) {
            QString variable = lock.cap(2).remove('"').trimmed();
            bool value = false;
            bool setLock = !(lock.cap(1).toLower() == "un");
            if (setLock)
                value = true;
            if (conf->get(variable) != NULL)
                conf->get(variable)->setLock(value);
            else
                qDebug() << "No lockable variable " << variable << "!";
        }
    }
    data.close();
    return 1;
}

void ScriptParser::execute(const QString &script, const QString &working, ParametersConfiguration *localData) {
    QProcess process;
    process.setWorkingDirectory(working);
    process.setStandardOutputFile(working + "/LOGS/" + script + ".log");
    process.start(executionCall, QIODevice::ReadOnly);
    process.waitForFinished(6 * (60 * 60 * 1000));
    parseResults(localData, working + "/LOGS/" + script + ".results");
    localData->save();
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
                else if (cell[0].trimmed() == "app_2dx_image")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::imageApp() + '"');
                else if (cell[0].trimmed() == "app_2dx_merge")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::mergeApp() + '"');
                else {
                    ParametersConfiguration* data = projectData.parameterData(workingDir);
                    if(data->get(cell[0].trimmed())) {
                        QString v = data->get(cell[0].trimmed())->value().toString();
                        scriptData[i].replace(QRegExp("\".*\""), '"' + v + '"');
                        scriptData.insert(++i, QString("echo \"") + cell[0].trimmed() + " = " + v + "\"\n");
                    }
                    else {
                        qDebug() << "Variable: " << cell[0].trimmed() << " not found.";
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
                else if (cell[0].trimmed() == "app_2dx_image")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::imageApp() + '"');
                else if (cell[0].trimmed() == "app_2dx_merge")
                    scriptData[i].replace(QRegExp("\"\\ *\""), '"' + ApplicationData::mergeApp() + '"');
                else {
                    ParametersConfiguration* data = projectData.parameterData(workingDir);
                    if(data->get(cell[0].trimmed())) {
                        QString v = data->get(cell[0].trimmed())->value().toString();
                        scriptData[i].replace(QRegExp("\".*\""), '"' + v + '"');
                        scriptData.insert(++i, QString("echo \"") + cell[0].trimmed() + " = " + v + "\"\n");
                    }
                    else {
                        qDebug() << "Variable: " << cell[0].trimmed() << " not found.";
                    }
                }
            }
        }
        i++;
    }

    if (i < scriptData.size()) scriptData.removeAt(i - 1);

    //Add the paths
    for (unsigned int ii = 0; ii < userPreferenceData.data()->size(); ii++) {
        if ((*userPreferenceData.data())[ii]->title().trimmed().toLower().startsWith("path")) {
            ParameterElementData *element;
            for (unsigned int j = 0; j < (*userPreferenceData.data())[ii]->size(); j++) {
                element = (*(*userPreferenceData.data())[ii])[j];
                scriptData.insert(i + j, "set " + element->name() + " = " + '"' + element->value().toString() + '"' + "\n");
            }
        }
    }
    
    return true;
}

