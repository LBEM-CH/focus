#include <confSectionHeader.h>

confSectionHeader::confSectionHeader(confData *conf, confSection *s, QWidget *parent)
                  :QFrame(parent)
{
  data = conf;
  section = s;  

  QHBoxLayout *layout = new QHBoxLayout;
  setLayout(layout);
  layout->setMargin(0); 
  layout->setSpacing(0);

  //setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
  //setLineWidth(1);

  setAutoFillBackground(true);
  QPalette titlePal(palette());
  QLinearGradient grad(QPoint(0,0),QPoint(0,height()));
  grad.setColorAt(1,QColor(31,92,207));
  grad.setColorAt(0.5,QColor(33,126,220));
  grad.setColorAt(0,QColor(88,153,229));
  titlePal.setBrush(QPalette::Background,grad);
  titlePal.setColor(QPalette::WindowText,QColor(247,245,250));
  setPalette(titlePal);

  setMinimumHeight(20);

  QIcon *icon = data->getIcon("expand");

  collapseButton = new graphicalButton(icon);
  collapseButton->setCheckable(true);
  collapseButton->setChecked(false);
  connect(collapseButton,SIGNAL(toggled(bool)),this,SIGNAL(hideChildren(bool)));
  connect(collapseButton,SIGNAL(toggled(bool)),this,SLOT(collapse(bool)));

  layout->addWidget(collapseButton);

  QLabel *title = new QLabel(s->title());
  QPalette headerPalette(palette());
  headerPalette.setBrush(QPalette::Background,grad);
  headerPalette.setColor(QPalette::WindowText,QColor(247,245,250));
  title->setPalette(headerPalette);
  title->setFixedHeight(20);
  title->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
  QFont titleFont(QApplication::font());
  titleFont.setPointSize(titleFont.pointSize()-1);
  titleFont.setWeight(QFont::Bold);
  title->setFont(titleFont);

  layout->addWidget(title);
  
  setChildrenHidden(false);

}

void confSectionHeader::setChildrenHidden(bool hidden)
{
  childrenHiddenValue = hidden;
}

bool confSectionHeader::childrenHidden()
{
  return childrenHiddenValue;
}

void confSectionHeader::addChild(confInput *input)
{
  childInputs<<input;
  visible[input] = true;
  connect(input,SIGNAL(shown()),this,SLOT(childShown()));
}

void confSectionHeader::collapse(bool value)
{
  if(value)
  {
    foreach(confInput *child, childInputs)
    {
      visible[child] = child->isVisible();
      child->hide();
    }
  }
  else
    foreach(confInput *child, childInputs)
      if(visible[child]) child->show();
  
}

void confSectionHeader::clearChildren()
{
  childInputs.clear();
}

void confSectionHeader::showAllChildren(bool value)
{
 foreach(confInput *child, childInputs)
    child->show();
}

void confSectionHeader::childShown()
{
  collapseButton->setChecked(false);
  show();
}


