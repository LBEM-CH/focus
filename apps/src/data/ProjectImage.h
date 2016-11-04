#ifndef IMAGEDATA_H
#define IMAGEDATA_H

#include <QObject>

#include "ParameterConfiguration.h"

class ProjectImage : public QObject {
    
    Q_OBJECT
    
public:

    ProjectImage(const QString& group, const QString directory, bool initParams = true, QObject* parent = 0);
    
    ParametersConfiguration* parameters();
    void reloadParameters();
    
    QString group();
    QString directory();
    QString workingPath();
    QDir workingDir();
    QString cfgFile();
    
    void setGroup(const QString& group);
    void setDirectory(const QString& dir);
    
    //Utility Methods
    void resetWithMasterConfig();
    void backup(int position);
    QString toString();
    
    //Static Methods
    static bool cfgFileExist(const QString& group, const QString& directory);
    
private:
    
    void initailizeFolder();
    
    QString group_;
    QString directory_;
    ParametersConfiguration* parameters_;

};

#endif /* IMAGEDATA_H */

