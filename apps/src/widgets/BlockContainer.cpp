#include "BlockContainer.h"

BlockContainer::BlockContainer(const QString& title, QWidget* mainWidget, QWidget* headerWidget, QWidget* parent)
: QFrame(parent) {
    setFrameStyle(QFrame::StyledPanel | QFrame::Plain);

    //setup title bar
    QWidget* header = setupHeader(title, headerWidget);

    //Setup contents
    QVBoxLayout* mainLayout = new QVBoxLayout(this);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);

    mainLayout->addWidget(header);
    mainLayout->addWidget(mainWidget);
    
    this->setLayout(mainLayout);
}

void BlockContainer::setHeaderTitle(const QString& titleLabel) {
    headerTitle->setText(titleLabel);
}

QWidget* BlockContainer::setupHeader(const QString& title, QWidget* headerWidget) {
    QWidget* bar = new QWidget(this);
    bar->setAutoFillBackground(true);

    //Set Height
    bar->setFixedHeight(20);

    //Setup Label
    headerTitle = new QLabel(title, this);
    headerTitle->setAutoFillBackground(false);
    headerTitle->setAlignment(Qt::AlignLeft);
    QPalette titlePal(headerTitle->palette());
    titlePal.setColor(QPalette::WindowText, Qt::darkGray);
    headerTitle->setPalette(titlePal);

    QFont font;
    font.setBold(true);
    headerTitle->setFont(font);

    //Setup spacer
    QSpacerItem *spacer = new QSpacerItem(bar->width(), bar->height(), QSizePolicy::Maximum);

    //setup layout
    QGridLayout* headerLayout = new QGridLayout(bar);
    headerLayout->setSpacing(0);
    headerLayout->setMargin(0);
    bar->setLayout(headerLayout);

    //Add widgets
    headerLayout->addItem(new QSpacerItem(3, 3), 0, 0, 1, 1);
    headerLayout->addWidget(headerTitle, 0, 1, 1, 1, Qt::AlignLeft | Qt::AlignVCenter);
    headerLayout->addItem(spacer, 0, 2, 1, 1);
    if(headerWidget) headerLayout->addWidget(headerWidget, 0, 3, 1, 1, Qt::AlignRight | Qt::AlignVCenter);

    return bar;

}

void BlockContainer::mouseDoubleClickEvent(QMouseEvent *event) {
    QWidget::mouseDoubleClickEvent(event);
    emit doubleClicked();
}