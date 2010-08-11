#include <confEditor.h>

confEditor::confEditor(confData *conf, QWidget *parent)
					 :QWidget(parent)
{
  data = conf; 
	setAttribute(Qt::WA_DeleteOnClose);
  setWindowModality(Qt::ApplicationModal);
  setWindowTitle("Preferences");

  QVBoxLayout *layout = new QVBoxLayout;

  QHBoxLayout *top = new QHBoxLayout;
  QHBoxLayout *middle = new QHBoxLayout;
  QHBoxLayout *bottom = new QHBoxLayout;

  layout->addLayout(top);
  layout->addLayout(middle);
  layout->addLayout(bottom);

  QFont font("Times New Roman",14,QFont::Bold);
  QLabel *title = new QLabel("Program Preferences (Double Click to Edit)",this);
  title->setFont(font);
  title->setTextFormat(Qt::RichText);
  top->addWidget(title);

  confModel *dataModel = new confModel(data,this);
  preferencesTable = new QTableView(this); 
  preferencesTable->setModel(dataModel);
  preferencesTable->horizontalHeader()->hide();
  preferencesTable->verticalHeader()->hide();
  preferencesTable->setColumnHidden(0,true);
  preferencesTable->setColumnHidden(2,true);
  preferencesTable->setRowHidden(0,true);
  preferencesTable->resizeColumnsToContents();
  preferencesTable->setShowGrid(false);
  preferencesTable->setAlternatingRowColors(true);
  middle->addWidget(preferencesTable);

  QPushButton *cancel = new QPushButton("Cancel",this);
  connect(cancel,SIGNAL(released()),this,SLOT(close()));
  QPushButton *saveButton = new QPushButton("Save",this);
  connect(saveButton,SIGNAL(released()),this,SLOT(save()));
  bottom->addStretch(2);
  bottom->addWidget(cancel);
  bottom->addWidget(saveButton);
  

  setLayout(layout);
  show();
  resize(500,250);
}

void confEditor::save()
{
  data->save();
  close();
}

void confEditor::resizeEvent(QResizeEvent *event)
{
  if(event->oldSize().width()>0)
	{
		if(event->size().width()>0)
		{
			float s = (float)(event->size().width())/(event->oldSize().width());
			int w1 = preferencesTable->columnWidth(1)*s;
			int w2 = preferencesTable->columnWidth(3)*s;

			if(w1+w2<preferencesTable->viewport()->width()) 
			{
				preferencesTable->resizeColumnToContents(1);
				preferencesTable->setColumnWidth(3,preferencesTable->viewport()->width()-preferencesTable->columnWidth(1));

			}
			else  
			{
				preferencesTable->setColumnWidth(1,w1);
				preferencesTable->setColumnWidth(3,w2);
			}
		}
	}
	else
	{
		preferencesTable->resizeColumnToContents(1);
		preferencesTable->resizeColumnToContents(3);
	}

	QWidget::resizeEvent(event);
}

