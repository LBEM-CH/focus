#ifndef PATHVALIDATOR_H
#define PATHVALIDATOR_H

#include <QValidator>
#include <QFile>

class FileValidator : public QValidator {
    
public:
    FileValidator(QObject* parent = 0) 
    : QValidator(parent){
    }
    
    QValidator::State validate(QString& input, int&) const override {
        QFile file(input);
        if(file.exists()) return QValidator::Acceptable;
        else return QValidator::Invalid;
    }

    
};

#endif /* PATHVALIDATOR_H */

