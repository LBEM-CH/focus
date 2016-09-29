#ifndef GROUPCONTAINER_H
#define GROUPCONTAINER_H

#include <QWidget>
#include <QFrame>
#include <QLabel>

class GroupContainer : public QWidget {
    
public:

    GroupContainer(QWidget* parent = 0) : QWidget(parent) {
        titleLabel_ = new QLabel;
        titleLabel_->setAlignment(Qt::AlignLeft);

        QFont f = titleLabel_->font();
        f.setCapitalization(QFont::Capitalize);
        f.setBold(true);
        titleLabel_->setFont(f);
        QPalette pal = titleLabel_->palette();
        pal.setColor(QPalette::WindowText, QColor(31, 92, 207));
        titleLabel_->setPalette(pal);

        container_ = new QFrame;
        container_->setFrameShadow(QFrame::Plain);
        container_->setFrameShape(QFrame::StyledPanel);
        
        QGridLayout* mainLayout = new QGridLayout;
        mainLayout->setSpacing(10);
        mainLayout->setMargin(10);
        mainLayout->addWidget(titleLabel_, 0, 0);
        mainLayout->addWidget(container_, 1, 0);
        
        setLayout(mainLayout);
    }

    void setTitle(const QString& title) {
        titleLabel_->setText(title);
    }

    void setContainerLayout(QLayout* layout) {
        container_->setLayout(layout);
    }

private:
    QVBoxLayout* mainLayout;
    QFrame* container_;
    QLabel* titleLabel_;
};


#endif /* GROUPCONTAINER_H */

