
#ifndef USER_DATA_H
#define USER_DATA_H

#include <QDebug>

#include "ApplicationData.h"
#include "ParameterConfiguration.h"

#define userPreferenceData (UserData::Instance())

class UserData {
    
    public:

    static UserData& Instance() {
        static UserData instance_;
        return instance_;
    }
    
    QString get(const QString& item) {
        return data_->get(item)->value().toString();
    }
    
    void set(const QString& item, const QString& value) {
        data_->setForce(item, value);
        data_->save();
    }
    
    ParametersConfiguration* data() {
        return data_;
    }
    
private:
    
    UserData() {
        data_ = new ParametersConfiguration(ApplicationData::configDir().absolutePath() + "/2dx.cfg", ApplicationData::homeDir().absolutePath() + "/2dx.cfg");
        if (data_->isEmpty()) {
            qDebug() << "2dx.cfg not found.";
            exit(0);
        }
        data_->save();
    }
    
    ParametersConfiguration* data_;
    
};

#endif /* USER_DATA_H */

