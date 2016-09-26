#ifndef PATHVALIDATOR_H
#define PATHVALIDATOR_H

#include <QValidator>
#include <QFileInfo>

class PathValidator : public QValidator {
    
public:
    PathValidator(QObject* parent = 0) 
    : QValidator(parent){
    }
    
    QValidator::State validate(QString& input, int&) const override {
        if(QFileInfo(input).exists()) return QValidator::Acceptable;
        else return QValidator::Invalid;
    }

    
};

#endif /* PATHVALIDATOR_H */

