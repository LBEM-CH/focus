#include <QtWidgets>

#include "ProjectData.h"
#include "SelectionCountHeader.h"

SelectionCountHeader::SelectionCountHeader(QWidget* parent)
: QWidget(parent){
    QWidget* header = setupHeader();
    
    connect(&projectData, &ProjectData::selectionChanged, [=] (const QList<ProjectImage*>& images) {
        setSelectionCount(images.count());
    });
    
    QGridLayout* mainLayout = new QGridLayout;
    setLayout(mainLayout);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(header, 0, 0);
    
    setSelectionCount(projectData.imagesSelected().count());
    setFixedHeight(35);
    update();
}

QWidget* SelectionCountHeader::setupHeader() {
    QFrame* bar = new QFrame(this);
    bar->setAutoFillBackground(true);
    bar->setFrameShadow(QFrame::Plain);
    bar->setFrameShape(QFrame::StyledPanel);
    
    
    //Setup Label
    headerTitle = new QLabel("", bar);
    headerTitle->setAutoFillBackground(false);
    headerTitle->setAlignment(Qt::AlignLeft);
    
    QFont font;
    font.setBold(true);
    font.setStretch(QFont::SemiExpanded);
    font.setCapitalization(QFont::Capitalize);
    headerTitle->setFont(font);
    
    //Setup header Widget
    selectionLabel = new QLabel;
    QPalette titlePal(selectionLabel->palette());
    titlePal.setColor(QPalette::WindowText, Qt::darkGray);
    selectionLabel->setPalette(titlePal);
    
    setSelectionCount(0);
    
    //Setup spacer
    QWidget* s = new QWidget();
    s->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    //setup layout
    QGridLayout* headerLayout = new QGridLayout(bar);
    headerLayout->setSpacing(3);
    headerLayout->setMargin(7);
    
    //Add widgets
    headerLayout->addItem(new QSpacerItem(3,3),0,0,1,1);
    headerLayout->addWidget(headerTitle, 0, 1, 1, 1, Qt::AlignLeft | Qt::AlignVCenter);
    headerLayout->addWidget(s, 0, 2, 1, 1);
    headerLayout->addWidget(selectionLabel, 0, 3, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
    
    bar->setLayout(headerLayout);
    
    return bar;
}

void SelectionCountHeader::setHeaderTitle(const QString& title) {
    headerTitle->setText(title.toUpper());
}


void SelectionCountHeader::setSelectionCount(int count) {
    selectionLabel->setText(QString::number(count) + " Images Selected");
}
