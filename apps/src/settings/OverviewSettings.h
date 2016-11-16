#ifndef OVERVIEWSETTINGS_H
#define OVERVIEWSETTINGS_H

#include <QSettings>
#include "ProjectPreferences.h"
#include "ApplicationData.h"

class OverviewSettings : public QSettings {
    
public:
    
    class OverviewProps {
    public:
        QStringList framePaths;
        QStringList frameTitles;
        QString title;
        QString overlay;
    };
    
    OverviewSettings() : 
    QSettings(ApplicationData::configDir().canonicalPath() + "/overviews.ini", QSettings::Format::IniFormat) {
    }
    
    QList<OverviewProps> overviews() {
        QList<OverviewProps> list;
        int size = beginReadArray("overviews");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            QString projectMode = QString::number(ProjectPreferences().projectMode());
            if(value("mode").toString() == "ALL" || value("mode").toString().contains(projectMode)) {
                OverviewProps props;
                props.title = value("title").toString();
                props.overlay = value("overlay").toString();
                int frames = beginReadArray("frame");
                for(int f = 0; f < frames; ++f) {
                    setArrayIndex(f);
                    props.framePaths.append(value("path").toString());
                    props.frameTitles.append(value("title").toString());
                }
                endArray();
                list.append(props);
            }
        }
        endArray();
        return list;
    }
    
};




#endif /* OVERVIEWSETTINGS_H */

