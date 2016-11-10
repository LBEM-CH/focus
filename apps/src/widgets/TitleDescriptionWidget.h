
#ifndef TITLEDESCRIPTIONWIDGET_H
#define TITLEDESCRIPTIONWIDGET_H

#include <QWidget>
#include <QLabel>
#include <QIcon>
#include <QFont>
#include <QGridLayout>

#include "GraphicalButton.h"

class TitleDescriptionWidget : public QWidget {
    
public:
    TitleDescriptionWidget(QWidget* parent = 0) :
    QWidget(parent){
    
        icon_ = new GraphicalButton(QIcon(), this);
        icon_->setFixedSize(34, 34);
        
        title_ = new QLabel();
        QFont titleFont = title_->font();
        titleFont.setBold(true);
        titleFont.setCapitalization(QFont::Capitalize);
        title_->setFont(titleFont);
        
        description_ = new QLabel();
        
        QGridLayout* mainlayout = new QGridLayout(this);
        mainlayout->setMargin(3);
        mainlayout->setSpacing(2);
        mainlayout->addWidget(icon_, 0, 0, 2, 1);
        mainlayout->addWidget(title_, 0, 1, 1, 1);
        mainlayout->addWidget(description_, 1, 1, 1, 1);
    }
    
    void setTitle(const QString& text) {
        title_->setText(text);
    }
    
    QString title() {
        return title_->text();
    }
    
    void setDescription(const QString& description) {
        description_->setText(description);
    }
    
    QString description() {
        return description_->text();
    }
    
    void setIcon(const QIcon& icon) {
        icon_->resetIcon(icon);
    }
    
private:
    QLabel* title_;
    QLabel* description_;
    GraphicalButton* icon_;
};

#endif /* PROJECTLISTWIDGET_H */

