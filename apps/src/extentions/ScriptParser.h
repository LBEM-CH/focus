#ifndef SCRIPTPARSER_H
#define SCRIPTPARSER_H

#include <QFile>
#include <QDir>
#include <QProcess>

#include "ParameterConfiguration.h"

class ScriptParser : public QObject {
    
    Q_OBJECT

public:
    ScriptParser(const QDir& workingDir, QObject* parent = 0);
    int parse(const QString &source, const QString &destination);
    void execute(const QString &script, const QString &working, ParametersConfiguration *localData);
    const QString &executionString();
    bool parseCsh(QStringList& scriptData);
    
    static bool parseResults(ParametersConfiguration* conf, const QString &results);
    static bool parseCsh(const QDir& workingDir, QStringList& scriptData);

private: 
    QDir workingDir;
    QString executionCall;


};

#endif
