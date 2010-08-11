#include <autoImportTool.h>

autoImportTool::autoImportTool(confData *importData, const QString &dir, QWidget *parent)
               :QWidget(parent)
{
  data = importData;
  importList = new importManager(data,this);
  QGridLayout *layout = new QGridLayout;
  setLayout(layout);
  directoryName = new QLineEdit(dir);
  layout->addWidget(directoryName,0,0,1,1);
  QPushButton *browseButton = new QPushButton("Browse");
  layout->addWidget(browseButton,0,1,1,1);
  console = new QLabel("Idle");
  QFrame *consoleFrame = new QFrame;
  consoleFrame->setFrameStyle(QFrame::Box);
  QGridLayout *subLayout = new QGridLayout;
  consoleFrame->setLayout(subLayout);
  subLayout->addWidget(console);
  layout->addWidget(consoleFrame,1,0,1,2);

  fileList = new QListWidget; 
  layout->addWidget(fileList,2,0,1,2);

  layout->setRowStretch(2,5);
  show();
  update(); 
}

void autoImportTool::update()
{
  QDir dir(directoryName->text());
  fileList->clear();
  if(!dir.exists() || directoryName->text().trimmed().isEmpty()) return;
  foreach (QString entry, dir.entryList(QDir::Files))
  {
    QString ext = QFileInfo(entry).suffix().trimmed().toLower();
    if(ext=="tif" || ext=="tiff" || ext=="mrc")
      fileList->addItem(entry); 
  }
}
