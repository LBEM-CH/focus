#include "ProjectData.h"
#include "ProjectPreferences.h"

ProjectPreferences::ProjectPreferences() : 
QSettings(projectData.projectDir().canonicalPath() + "/merge/config/project.preferences.ini", QSettings::Format::IniFormat) {
}

ProjectPreferences::ProjectPreferences(const QDir& projectDir): 
QSettings(projectDir.canonicalPath() + "/merge/config/project.preferences.ini", QSettings::Format::IniFormat) {
}


QStringList ProjectPreferences::imagesOpen() {
    QStringList paths;
    int size = beginReadArray("open");
    for (int i = 0; i < size; ++i) {
        setArrayIndex(i);
        if (value("path").toString() != "") paths << value("path").toString();
    }
    endArray();
    return paths;
}

void ProjectPreferences::setImagesOpen(const QStringList& paths) {
    remove("open");
    beginWriteArray("open");
    for (int i = 0; i < paths.size(); ++i) {
        setArrayIndex(i);
        setValue("path", paths.at(i));
    }
    endArray();
}

QString ProjectPreferences::projectName() {
    QString name;
    beginGroup("project");
    name = value("name", "Unnamed Project").toString();
    endGroup();
    return name;
}

void ProjectPreferences::setProjectName(const QString& name) {
    beginGroup("project");
    setValue("name", name);
    endGroup();
}

int ProjectPreferences::projectMode() {
    int mode;
    beginGroup("project");
    mode = value("mode", 1).toInt();
    endGroup();
    return mode;
}

void ProjectPreferences::setProjectMode(int mode) {
    beginGroup("project");
    setValue("mode", mode);
    endGroup();
}

int ProjectPreferences::overviewIndex() {
    int val;
    beginGroup("project");
    val = value("overviewIndex", 0).toInt();
    endGroup();
    return val;
}

void ProjectPreferences::setOverviewIndex(int jobs) {
    beginGroup("project");
    setValue("overviewIndex", jobs);
    endGroup();
}

bool ProjectPreferences::importRestartCheck() {
    bool val;
    beginGroup("import");
    val = value("onRestart", false).toBool();
    endGroup();
    return val;
}

void ProjectPreferences::setImportRestartCheck(bool check) {
    beginGroup("import");
    setValue("onRestart", check);
    endGroup();
}

int ProjectPreferences::importSafeInterval() {
    int val;
    beginGroup("import");
    val = value("safe_interval", 180).toInt();
    endGroup();
    return val;
}

void ProjectPreferences::setImportSafeInterval(int seconds) {
    beginGroup("import");
    setValue("safe_interval", seconds);
    endGroup();
}

bool ProjectPreferences::importDeleteCheck() {
    bool val;
    beginGroup("import");
    val = value("delete_original", false).toBool();
    endGroup();
    return val;
}

void ProjectPreferences::setImportDeleteCheck(bool check) {
    beginGroup("import");
    setValue("delete_original", check);
    endGroup();
}

bool ProjectPreferences::processAutoCheck() {
    bool val;
    beginGroup("process");
    val = value("auto_start", false).toBool();
    endGroup();
    return val;
}

void ProjectPreferences::setProcessAutoCheck(bool check) {
    beginGroup("process");
    setValue("auto_start", check);
    endGroup();
}

int ProjectPreferences::processJobs() {
    int val;
    beginGroup("process");
    val = value("jobs", 2).toInt();
    endGroup();
    return val;
}

void ProjectPreferences::setProcessJobs(int jobs) {
    beginGroup("process");
    setValue("jobs", jobs);
    endGroup();
}

QStringList ProjectPreferences::processScripts() {
    beginGroup("process");
    QStringList paths;
    int size = beginReadArray("scripts");
    for (int i = 0; i < size; ++i) {
        setArrayIndex(i);
        if (value("name").toString() != "") paths << value("name").toString();
    }
    endArray();
    endGroup();
    return paths;
}

void ProjectPreferences::setProcessScripts(const QStringList& scripts) {
    beginGroup("process");
    remove("scripts");
    beginWriteArray("scripts");
    for (int i = 0; i < scripts.size(); ++i) {
        setArrayIndex(i);
        setValue("name", scripts.at(i));
    }
    endArray();
    endGroup();
}