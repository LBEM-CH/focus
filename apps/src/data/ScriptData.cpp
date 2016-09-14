#include <QDebug>

#include "ProjectData.h"
#include "ApplicationData.h"

#include "ScriptData.h"

ScriptData::ScriptData(const QString& filename, QObject* parent)
: QObject(parent){
    parseDataFile(filename);
}

QString& ScriptData::parseVariables(QString& line) {
    line.replace("${appDir_2dx}", ApplicationData::applicationDir().absolutePath());
    line.replace("${proc_2dx}", ApplicationData::procScriptsDir().absolutePath());
    line.replace("${app_2dx_image}", ApplicationData::imageApp());
    line.replace("${app_2dx_merge}", ApplicationData::mergeApp());
    return line;
}

bool ScriptData::parseDataFile(const QString& filename) {
    QFile data(filename);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

    QString lineData;
    qint64 pos = -1;
    while (!data.atEnd() && pos != data.pos() && lineData.toLower() != "$end_local_vars" && lineData.toLower() != "$end_vars") {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();
        if (lineData.toLower().startsWith("manual:")) {
            lineData.remove(0, 7);
            parseVariables(lineData);
            manualData << lineData.trimmed();
        }

        if (lineData.contains(':')) {
            QStringList cell = lineData.split(':');
            if (cell.first().trimmed().toLower() == "global") {
                cell.first() = "display";
            }
            if (cell.first().trimmed().toLower() == "reset") {
                QStringList val = cell.last().split('=');
                val[1].remove('"');
                val[0] = val[0].simplified();
                val[1] = val[1].simplified();
                resetVars.insert(val[0], val[1]);
                cell.first() = "display";
                cell.last() = val[0];
            }
            properties.insert(cell.first().trimmed().toLower(), cell.last().trimmed());
        }
    }

    QRegExp sep1("^\\s*python\\s+\\$\\{proc_2dx\\}\\/(\\S+\\.py).+");
    QRegExp sep2("^\\s*source\\s+\\$\\{proc_2dx\\}\\/(\\S+)");
    QRegExp sep3("^\\s*\\$\\{app_python\\}\\s+\\$\\{proc_2dx\\}\\/(\\S+\\.py).+");
    QString seplineData;
    while (!data.atEnd()) {
        seplineData = data.readLine();
        if (sep1.indexIn(seplineData) != -1)
            subScript << sep1.cap(1);
        if (sep2.indexIn(seplineData) != -1)
            subScript << sep2.cap(1);
        if (sep3.indexIn(seplineData) != -1)
            subScript << sep3.cap(1);
    }

    data.close();
    return true;
}

QStringList& ScriptData::manual() {
    return manualData;
}

const QString& ScriptData::initializationScript(bool fullName) {
    if (fullName)
        return initializationScriptName;
    else
        return initializationScriptBaseName;
}

QString ScriptData::property(const QString &propertyName) {
    return properties.value(propertyName.toLower());
}

QList<QString> ScriptData::propertyList(const QString &propertyName) {
    return properties.values(propertyName.toLower());
}

QMap<QString, QString> ScriptData::resetVariables() {
    return resetVars;
}

const QSet<QString> &ScriptData::subScripts() {
    return subScript;
}