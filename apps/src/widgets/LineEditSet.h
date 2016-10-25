#ifndef EDIT_SET_WIDGET_HPP
#define EDIT_SET_WIDGET_HPP

#include <QWidget>
#include <QLineEdit>
#include <QList>
#include <QHBoxLayout>
#include <QString>
#include <QStringList>
#include <QValidator>
#include <QIntValidator>
#include <QDoubleValidator>
#include <QPair>
#include <QDebug>

class LineEditSet : public QWidget {

    Q_OBJECT

public:

    LineEditSet(int count = 1, QWidget* parent = NULL)
    : QWidget(parent) {
        QHBoxLayout* layout = new QHBoxLayout(this);
        layout->setMargin(0);

        for (int i = 0; i < count; ++i) {
            QLineEdit* widget = new QLineEdit(this);
            widget->setFrame(false);
            widgets_.append(widget);
            connect(widget, &QLineEdit::textEdited, this, &LineEditSet::updateValues);
            layout->addWidget(widget, 1);
        }

        setLayout(layout);
    }

    QList<QLineEdit*> widgets() {
        return widgets_;
    }

    QString valueAt(int widgetNumber) {
        if (widgetNumber < widgets_.size()) return widgets_.at(widgetNumber)->text();
        else return QString();
    }

public slots:

    void updateValues() {
        QString v;
        for (int i = 0; i < widgets_.size(); i++) {
            if(i != 0) v += ',';
            v += widgets_[i]->text().trimmed();
            QPalette pal = widgets_[i]->palette();
            if(widgets_[i]->hasAcceptableInput()) pal.setColor(QPalette::Text, Qt::black);
            else pal.setColor(QPalette::Text, Qt::red);
            widgets_[i]->setPalette(pal);
        }

        emit valueChanged(v);
    }

    void setValue(const QString& value) {
        if(widgets_.size() == 1) {
            widgets_[0]->setText(value);
            QPalette pal = widgets_[0]->palette();
            if(widgets_[0]->hasAcceptableInput()) pal.setColor(QPalette::Text, Qt::black);
            else pal.setColor(QPalette::Text, Qt::red);
            widgets_[0]->setPalette(pal);
            return;
        }
        QStringList vals = value.trimmed().split(',');
        if(vals.size() != widgets_.size()) {
            qDebug() << "WARNING: Trying to set value of element. Got " << vals.size() << " values. But expected: " << widgets_.size();
        }
        for (int i = 0; i < vals.size(); i++) {
            if (i < widgets_.size()) {
                widgets_[i]->setText(vals[i]);
                QPalette pal = widgets_[i]->palette();
                if(widgets_[i]->hasAcceptableInput()) pal.setColor(QPalette::Text, Qt::black);
                else pal.setColor(QPalette::Text, Qt::red);
                widgets_[i]->setPalette(pal);
            }
        }
    }

signals:

    void valueChanged(const QString& value);

private:
    QList<QLineEdit*> widgets_;

};

class IntLineEditSet : public LineEditSet {
public:

    IntLineEditSet(int count = 1, QWidget* parent = NULL)
    : LineEditSet(count, parent) {
        QLineEdit* member;

        foreach(member, widgets()) {
            member->setValidator(new QIntValidator(this));
        }
    }

    void setRange(QMap<int, QStringList> widgetRange) {

        foreach(int i, widgetRange.keys()) {
            if (i < widgets().size()) {
                QIntValidator* validator = new QIntValidator();
                if (!widgetRange.value(i)[0].isEmpty()) validator->setBottom(widgetRange.value(i)[0].toInt());
                if (widgetRange.value(i).size() > 1 && !widgetRange.value(i)[1].isEmpty()) validator->setTop(widgetRange.value(i)[1].toInt());
                widgets()[i]->setValidator(validator);
            }
        }
    }

    void setAllRanges(int min, int max) {

        foreach(QLineEdit* widget, widgets()) {
            QIntValidator* validator = new QIntValidator(this);
            validator->setRange(min, max);
            widget->setValidator(validator);
        }
    }

};

class DoubleLineEditSet : public LineEditSet {
public:

    DoubleLineEditSet(int count = 1, QWidget* parent = NULL)
    : LineEditSet(count, parent) {
        QLineEdit* member;

        foreach(member, widgets()) {
            member->setValidator(new QDoubleValidator(this));
        }
    }

    void setRange(QMap<int, QStringList> widgetRange) {

        foreach(int i, widgetRange.keys()) {
            if (i < widgets().size()) {
                QDoubleValidator* validator = new QDoubleValidator(this);
                if (!widgetRange.value(i)[0].isEmpty()) validator->setBottom(widgetRange.value(i)[0].toDouble());
                if (widgetRange.value(i).size() > 1 && !widgetRange.value(i)[1].isEmpty()) validator->setTop(widgetRange.value(i)[1].toDouble());
                widgets()[i]->setValidator(validator);
            }
        }
    }

};


#endif /* EDIT_SET_WIDGET_HPP */
