#ifndef DIRVALIDATOR_H
#define DIRVALIDATOR_H

#include <QValidator>
#include <QDir>

class DirValidator : public QValidator {
    
public:
    DirValidator(QObject* parent = 0) 
    : QValidator(parent){
    }
    
    QValidator::State validate(QString& input, int&) const override {
        QDir dir(input);
        if(dir.exists()) return QValidator::Acceptable;
        else return QValidator::Invalid;
    }

    
};

#endif /* PATHVALIDATOR_H */

