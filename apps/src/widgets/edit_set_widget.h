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

class EditSetWidget : public QWidget {

    Q_OBJECT

public:

    EditSetWidget(int count = 1, QWidget* parent = NULL)
    : QWidget(parent) {
        QHBoxLayout* layout = new QHBoxLayout(this);
        layout->setMargin(0);

        for (int i = 0; i < count; ++i) {
            QLineEdit* widget = new QLineEdit(this);
            widget->setFrame(false);
            widgets_.append(widget);
            connect(widget, SIGNAL(editingFinished()), this, SLOT(updateValues()));
            layout->addWidget(widget, 1);
        }

        setLayout(layout);
    }

    QList<QLineEdit*> widgets() {
        return widgets_;
    }

    QString valueAt(int widgetNumber) {
        if (widgetNumber < widgets_.size()) widgets_.at(widgetNumber)->text();
        else return QString();
    }

public slots:

    void updateValues() {
        QString v;
        for (int i = 0; i < widgets_.size() - 1; i++)
            v += widgets_[i]->text() + ',';
        v += widgets_.last()->text();

        emit valueChanged(v);
    }

    void setValue(const QString& value) {
        QStringList vals = value.trimmed().split(',');
        for (int i = 0; i < vals.size(); i++) {
            if (i < widgets_.size()) widgets_[i]->setText(vals[i]);
        }
    }

signals:

    void valueChanged(const QString& value);

private:
    QList<QLineEdit*> widgets_;

};

class EditIntSetWidget : public EditSetWidget {
public:

    EditIntSetWidget(int count = 1, QWidget* parent = NULL)
    : EditSetWidget(count, parent) {
        QLineEdit* member;

        foreach(member, widgets()) {
            member->setValidator(new QIntValidator());
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
            QIntValidator* validator = new QIntValidator();
            validator->setRange(min, max);
            widget->setValidator(validator);
        }
    }

};

class EditDoubleSetWidget : public EditSetWidget {
public:

    EditDoubleSetWidget(int count = 1, QWidget* parent = NULL)
    : EditSetWidget(count, parent) {
        QLineEdit* member;

        foreach(member, widgets()) {
            member->setValidator(new QDoubleValidator());
        }
    }

    void setRange(QMap<int, QStringList> widgetRange) {

        foreach(int i, widgetRange.keys()) {
            if (i < widgets().size()) {
                QDoubleValidator* validator = new QDoubleValidator();
                if (!widgetRange.value(i)[0].isEmpty()) validator->setBottom(widgetRange.value(i)[0].toDouble());
                if (widgetRange.value(i).size() > 1 && !widgetRange.value(i)[1].isEmpty()) validator->setTop(widgetRange.value(i)[1].toDouble());
                widgets()[i]->setValidator(validator);
            }
        }
    }

};


#endif /* EDIT_SET_WIDGET_HPP */
