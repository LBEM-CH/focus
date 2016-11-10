#ifndef PROJECTMODE_H
#define PROJECTMODE_H

#include <QObject>
#include <QString>

#include "ApplicationData.h"

class ProjectMode {
public:
    
    enum class Mode {
        DRIFT_CORRECTION, TWOD_CRYSTALLOGRAPHY, SINGLE_PARTICLE
    };

    ProjectMode(int modeId) {
        if(modeId == static_cast<int>(Mode::DRIFT_CORRECTION)) mode_ = Mode::DRIFT_CORRECTION;
        else if(modeId == static_cast<int>(Mode::TWOD_CRYSTALLOGRAPHY)) mode_ = Mode::TWOD_CRYSTALLOGRAPHY;
        else if(modeId == static_cast<int>(Mode::SINGLE_PARTICLE)) mode_ = Mode::SINGLE_PARTICLE;
        else {
            qDebug() << "WARNING: Mode could not be initialized from the id: " << modeId;
            qDebug() << "WARNING: Using Drift correction mode.";
            mode_ = Mode::DRIFT_CORRECTION;
        }
    }
    
    QIcon getIcon() {
        if(mode_ == Mode::DRIFT_CORRECTION) return ApplicationData::icon("mode_drift");
        else if(mode_ == Mode::TWOD_CRYSTALLOGRAPHY) return ApplicationData::icon("mode_2dx");
        else if(mode_ == Mode::SINGLE_PARTICLE) return ApplicationData::icon("mode_sp");
        else return QIcon();
    }
    
    QString toString() {
        if(mode_ == Mode::DRIFT_CORRECTION) return QString("Drift Correction");
        else if(mode_ == Mode::TWOD_CRYSTALLOGRAPHY) return QString("2D Electron Crystallography");
        else if(mode_ == Mode::SINGLE_PARTICLE) return QString("Single Particle");
        else return QString();
    }
    
    int toInt() {
        return static_cast<int>(mode_);
    }
    
    void setModeFromString(const QString& mode) {
        if(mode == "Drift Correction") mode_ = Mode::DRIFT_CORRECTION;
        else if(mode == "2D Electron Crystallography") mode_ = Mode::TWOD_CRYSTALLOGRAPHY;
        else if(mode == "Single Particle") mode_ = Mode::SINGLE_PARTICLE;
        else {
            qDebug() << "WARNING: Mode could not be changed from the string: " << mode;
        }
    }
    
    static QList<ProjectMode> availableModes() {
        QList<ProjectMode> modes;
        for(int i=0; i<=2; ++i) {
            modes.append(ProjectMode(i));
        }
        
        return modes;
    }
    
private:
    Mode mode_;

};

#endif /* PROJECTMODE_H */

