#include "ScriptHelp.h"

ScriptHelp::ScriptHelp(QWidget* parent)
: QDialog(parent) {
    setWindowTitle("Focus | Script Help");
    setModal(true);
    resize(500, 400);

    defaultPublications = ApplicationData::defaultPublicationsList();

    setupTitleLabel();
    setupManualLabel();

    setMinimumHeight(150);

    QVBoxLayout *mainLayout = new QVBoxLayout(this);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addStretch(0);
    mainLayout->addWidget(title, 0);
    mainLayout->addWidget(browser, 1);

    setLayout(mainLayout);

}

void ScriptHelp::setData(const QStringList& manual, const QStringList& scriptPubs) {
    browser->clear();
    browser->insertHtml("<B>MANUAL</B><p>");
    for (int i = 0; i < manual.size(); i++) browser->insertHtml(manual[i] + "<p>");
    browser->insertHtml("<br><B>FURTHER READING</B><p>");
    browser->insertHtml("<I>(Please cite the publications in the following list if you use this script)</I><p>");
    for (int i = 0; i < scriptPubs.size(); i++) browser->insertHtml(scriptPubs[i] + "<p>");
    for (int i = 0; i < defaultPublications.size(); i++) browser->insertHtml(defaultPublications[i] + "<p>");
    browser->moveCursor(QTextCursor::Start);
}

void ScriptHelp::setTitle(const QString& t) {
    title->setText(t);
}

void ScriptHelp::setupManualLabel() {
    browser = new TextBrowser(this);
    browser->setReadOnly(true);
    browser->moveCursor(QTextCursor::Start);
    QPalette pal(browser->palette());
    pal.setColor(QPalette::Base, QColor(255, 255, 220));
    browser->setPalette(pal);
}

void ScriptHelp::setupTitleLabel() {
    title = new QLabel("Script Name", this);
    title->setAutoFillBackground(true);
    QPalette titlePal(palette());
    QLinearGradient grad(QPoint(0, 0), QPoint(0, title->height()));
    grad.setColorAt(1, QColor(207, 92, 31));
    grad.setColorAt(0.5, QColor(220, 126, 33));
    grad.setColorAt(0, QColor(229, 153, 88));
    ;

    titlePal.setBrush(QPalette::Background, grad);
    titlePal.setColor(QPalette::WindowText, QColor(247, 245, 250));

    title->setPalette(titlePal);
    title->setFixedHeight(20);
    title->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    QFont titleFont = title->font();
    titleFont.setPointSize(16);
    titleFont.setWeight(QFont::Bold);
    title->setFont(titleFont);
}