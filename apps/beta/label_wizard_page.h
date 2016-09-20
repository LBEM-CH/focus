/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   label_page.hpp
 * Author: biyanin
 *
 * Created on June 10, 2016, 8:32 PM
 */

#ifndef LABEL_PAGE_HPP
#define LABEL_PAGE_HPP

#include <QWizardPage>
#include <QString>
#include <QLabel>
#include <QVBoxLayout>

class LabelWizardPage : public QWizardPage {

    Q_OBJECT

public:
    LabelWizardPage(const QString& labelText = "", QWidget* parent = NULL)
    : QWizardPage(parent) {
        QVBoxLayout* layout = new QVBoxLayout();
        label = new QLabel(labelText);
        label->setWordWrap(true);
        layout->addWidget(label);
        setLayout(layout);
    }

    void setLabelText(const QString& text) {
        setLabelText(text);
    }

private:
    QLabel* label;

};


#endif /* LABEL_PAGE_HPP */

