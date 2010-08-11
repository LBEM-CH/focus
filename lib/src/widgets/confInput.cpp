/*
 *  confInput.cpp
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "confInput.h"
#include <iostream>
#include <QListView>
#include <QApplication>
#include <QDebug>
using namespace std;

confInput::confInput(confData *conf, confElement *e, QWidget *parent)
                    :QWidget(parent)
{
  QString userLevelString = e->get("USERLEVEL").trimmed().toLower();
  if(userLevelString == "advanced")
    user_level = 1;
  else
    user_level = 0;

  setAutoFillBackground(true);

  const int LINE_EDIT_WIDTH=150;
  const int LINE_EDIT_HEIGHT=20;

  data = conf;
  setSizePolicy(QSizePolicy::Minimum,QSizePolicy::Maximum);
  layout = new QGridLayout(this);
  setLayout(layout);
  layout->setMargin(1);
  layout->setSpacing(2);
  layout->setAlignment(Qt::AlignLeft);
  element = e;
  if(element == NULL) element = new confElement;
  varUID = element->get("valuelabel");

  lockedBox = NULL;
  menu = NULL;
  yes = NULL; no = NULL;
  yesLabel = NULL; noLabel = NULL;

  updateWhatsThis();

  QStringList type_default=element->get("type").split('"');
  type = type_default[0].trimmed().toLower();
  QString defaults;
  if(type_default.size()>1)
    defaults = type_default[1].trimmed();
  QString locked = element->get("locked").toLower().trimmed();
  QString value = element->get("value");

  labelFont = font();
  labelFont.setPointSize(labelFont.pointSize()-1);
  label = new QLabel(element->get("label").simplified() + ": ",this);
  label->setFont(labelFont);
  setFont(labelFont);

  if(type == "fourtynine_float")
    layout->addWidget(label,0,0,1,1);
  else
  {
    layout->addWidget(label,0,0,1,1);
    layout->setColumnStretch(1,2);
  }

  if(type=="text_edit" || type == "integer" || type == "float")
  {
    QStringList range = defaults.remove('"').split(';');
    float min=-FLT_MAX, max=FLT_MAX;
    if(type == "text_edit") value = element->get("value");
    if(type == "integer") value = QString::number(element->get("value").toInt());
    if(type == "float") value = QString::number(element->get("value").toFloat());

    lEdits << new QLineEdit(value,this);
    layout->setColumnStretch(2,1);
    if(type=="text_edit" && range.size()==2) 
		{
      float w = range.first().toFloat(), h = range.last().toFloat();
      if(w <= 0.0) w = 1.0;
      if(h <= 0.0) h = 1.0;
    
			lEdits[0]->setFixedWidth(int(LINE_EDIT_WIDTH*w));
			lEdits[0]->setFixedHeight(int(LINE_EDIT_HEIGHT*h));

      if(h!=1.0) label->setAlignment(Qt::AlignLeft | Qt::AlignTop);
		}
		else
		{
			lEdits[0]->setFixedWidth(LINE_EDIT_WIDTH);
			lEdits[0]->setFixedHeight(LINE_EDIT_HEIGHT);
		}
		lEdits[0]->setAutoFillBackground(true);
		lEdits[0]->setFont(labelFont);

		if(type!="text_edit")
		{
			foreach(QString s, range)
			{
				QStringList rangeValue = s.remove('"').split('=');
				if(rangeValue.first().trimmed().toLower().startsWith("min"))
					min = rangeValue.last().toFloat();
				else if(rangeValue.first().trimmed().toLower().startsWith("max"))
					max = rangeValue.last().toFloat();
			}

			//confValidator *v = new confValidator(lEdits[0]);
			//v->setRange(min,max);
			//lEdits[0]->setValidator(v);
		}

		layout->addWidget(lEdits[0],0,3,1,1);
		connect(lEdits[0],SIGNAL(textEdited(const QString &)),this,SLOT(save()));
	}

	if(type == "two_float" || type == "three_float" || type == "four_float" || type == "fourtynine_float")
	{
		int k = 0;
		QList<float> min, max;
		float lower, upper;

		QStringList range = defaults.split(';');
		foreach(QString s, range)
		{
			QStringList rangeValue = s.remove('"').split('=');
			if(rangeValue.first().trimmed().toLower().startsWith("min"))
				min << rangeValue.last().toFloat();
			else if(rangeValue.first().trimmed().toLower().startsWith("max"))
				max << rangeValue.last().toFloat();
		}

		if(type == "two_float") k = 2;
		if(type == "three_float") k = 3;
		if(type == "four_float") k = 4;
		if(type == "fourtynine_float") k = 49;

		QStringList fValues = element->get("value").split(',');
		for(int i=0;i<k;i++)
		{
			if(i<fValues.size())
				lEdits << new QLineEdit(fValues[i],this);
			else
				lEdits << new QLineEdit(this);
			lEdits[i]->setFixedHeight(20);

			if(i<min.size()) lower = min[i]; else lower = min.last();
			if(i<max.size()) upper = max[i]; else upper = max.last();

			if(!min.isEmpty() && !max.isEmpty())
			{
				//confValidator *v = new confValidator(lEdits[i]);
				//v->setRange(lower,upper);
				//lEdits[i]->setValidator(v);
			}

			//if(type!="fourtynine_float") 
			lEdits[i]->setMaximumWidth(LINE_EDIT_WIDTH/2);
			//else lEdits[i]->setMaximumWidth(LINE_EDIT_WIDTH/(2));

			if(type!="fourtynine_float")
				layout->addWidget(lEdits[i],0,i+3,1,1);
			else
				layout->addWidget(lEdits[i],1+i/7,i%7,1,1);
			connect(lEdits[i],SIGNAL(textEdited(const QString &)),this,SLOT(save()));
		}

		if(type=="fourtynine_float")
		{
			QPalette pal(palette());
			pal.setColor(QPalette::Base,QColor(125,125,255));
			lEdits[24]->setPalette(pal);
		}
	}

	if(type == "drop_down_menu")
	{
		menu = new QComboBox(this);
		menu->setSizeAdjustPolicy(QComboBox::AdjustToContents);
		int k;
		bool ok;
		QStringList menuOptions = defaults.split(';');
		for(int i=0;i<menuOptions.size();i++)
		{
			QStringList menuItem = menuOptions[i].split('=');
			menu->addItem(menuItem.last());
		}

		k = value.toInt(&ok);
		if(!ok || k<0)
		{
			if(menuOptions.contains(QString::number(k))) k = menuOptions.indexOf(value);
		}
		else k=0;

		if(k>=0)
			menu->setCurrentIndex(k);
		else
			menu->setCurrentIndex(0);

		layout->addWidget(menu,0,3,1,1);

		connect(menu,SIGNAL(currentIndexChanged(int)),this,SLOT(save()));
	}

	if(type == "bool")
	{
		QButtonGroup *buttonGroup = new QButtonGroup(this);
		yes = new QRadioButton(this);
		yesLabel = new QLabel("Yes");
		no = new QRadioButton(this);
		noLabel = new QLabel("No");
		buttonGroup->addButton(yes);
		buttonGroup->addButton(no);
		buttonGroup->setExclusive(true);
		if(value.toLower()=="y") yes->setChecked(true);
		else if(value.toLower()=="n") no->setChecked(true);
		layout->addWidget(yes,0,3,1,1);
		layout->addWidget(yesLabel,0,4,1,1);
		layout->addWidget(no,0,5,1,1);
		layout->addWidget(noLabel,0,6,1,1);
		yes->setFixedHeight(20);
		//layout->setAlignment(yes,Qt::AlignRight);
		//layout->setAlignment(no,Qt::AlignRight);
		connect(buttonGroup,SIGNAL(buttonClicked(int)),this,SLOT(save()));
	}

	if(!locked.isEmpty())
	{
		lockedBox = new graphicalButton(data->getIcon("lock"),this);
		lockedBox->setCheckable(true);
		lockedBox->setFixedHeight(20);
		if(locked.startsWith("yes")) {lockedBox->setChecked(true); setReadOnlyState(Qt::Checked);}
		else {lockedBox->setChecked(false); setReadOnlyState(Qt::Unchecked);}
		if(type == "fourtynine_float")
			layout->addWidget(lockedBox,0,2,1,6);
		else
			layout->addWidget(lockedBox,0,2,1,1);
		layout->setAlignment(lockedBox,Qt::AlignRight | Qt::AlignVCenter);
		connect(lockedBox,SIGNAL(stateChanged(int)),this,SLOT(setReadOnlyState(int)));
		connect(lockedBox,SIGNAL(stateChanged(int)),this,SLOT(save()));
	}
	updateFontInfo();
	//cerr<<connect(element,SIGNAL(dataChanged()),this,SLOT(load()))<<endl;
	//cerr<<connect(data,SIGNAL(loading()),this,SLOT(load()))<<endl;
}

void confInput::save()
{
	bool change = false;
	QStringList type_default=element->get("type").split('"');
	QString type = type_default[0].trimmed().toLower();

	if(type=="text_edit" || type == "integer" || type == "float")
	{
		change = element->set("value",lEdits[0]->text()) || change;
	}

	if(type == "two_float" || type == "three_float" || type == "four_float" || type == "fourtynine_float")
	{
		QString v;
		for(int i=0;i<lEdits.size()-1;i++)
			v+=lEdits[i]->text() + ',';
		v+=lEdits.last()->text();

		change = element->set("value",v) || change;
	}

	if(type == "drop_down_menu")
	{
		if(type_default.size()>1)
		{
			QStringList defaults = type_default[1].trimmed().split(';');
      if(menu->currentIndex()>=0)
      {
			  QStringList option = defaults[menu->currentIndex()].split('=');
        if(!option.isEmpty())
		  		change = element->set("value",option[0]) || change;
        else
          qDebug()<<element->get("valuelabel")<<" has no value!";
      }
      else qDebug()<<"Menu for "<<element->get("valuelabel")<<" has no index selected!";
		}
	}

	if(type == "bool")
	{
		if(yes->isChecked()) element->set("value","y") || change;
		else if(no->isChecked()) element->set("value","n") || change;
		else change = element->set("value","") || change;
	}

	if(lockedBox!=NULL)
	{
		if(lockedBox->isChecked()) element->set("locked","YES") || change;
		else change = element->set("locked","NO") || change;
	}
	dataModified();
	if(change) updateStatus();
	updateFontInfo();
}

void confInput::load()
{
	//cerr<<"Loading.."<<endl;
	QStringList type_default=element->get("type").split('"');
	QString type = type_default[0].trimmed().toLower();
	QString defaults;
	if(type_default.size()>1)
		defaults = type_default[1].trimmed();
	QString value = element->get("value");
	QString locked = element->get("locked").toLower().trimmed();

	if(type=="text_edit" || type == "integer" || type == "float")
	{
		lEdits[0]->setText(value);
	}

	if(type == "two_float" || type == "three_float" || type == "four_float" || type == "fourtynine_float")
	{
		QStringList fValues = value.split(',');
		QString value;
		for(int i=0;i<fValues.size();i++)
		{
			if(type == "fourtynine_float")
				value = QString::number(int(fValues[i].trimmed().toFloat()));
			else
				value = fValues[i];
			lEdits[i]->setText(value);
		}
	}

	if(type == "drop_down_menu")
	{
		QStringList menuOptions = defaults.split(';');
		bool ok;
		int k = value.toInt(&ok);
		if(!ok || menuOptions.contains(QString::number(k))) k = menuOptions.indexOf(value);
		if(k==-1) cerr<<element->get("label").toStdString()<<" contains no option \""<<value.toStdString()<<"\""<<endl;
		else menu->setCurrentIndex(k);
	}

	if(type == "bool")
	{
		if(value.toLower()=="y") yes->setChecked(true);
		else if(value.toLower()=="n") no->setChecked(true);
	}

	if(!locked.isEmpty())
	{
		if(locked.startsWith("yes")) {lockedBox->setChecked(true); setReadOnlyState(Qt::Checked);}
		else {lockedBox->setChecked(false); setReadOnlyState(Qt::Unchecked);}
	}

	//repaint();
  update();
}

void confInput::setReadOnlyState(int state)
{
	if(state == Qt::Checked)
	{
		for(int i=0;i<lEdits.size();i++)
			lEdits[i]->setDisabled(true);

		if(menu!=NULL)
			menu->setDisabled(true);

		if(yes!=NULL && no!=NULL)
		{
			yes->setDisabled(true);
			no->setDisabled(true);
		}
	}
	else if(state == Qt::Unchecked)
	{
		for(int i=0;i<lEdits.size();i++)
			lEdits[i]->setDisabled(false);

		if(menu!=NULL)
			menu->setDisabled(false);

		if(yes!=NULL && no!=NULL)
		{
			yes->setDisabled(false);
			no->setDisabled(false);
		}
	}
}

void confInput::mousePressEvent(QMouseEvent *event)
{
	if(event->button() == Qt::RightButton)
		QWhatsThis::showText(event->globalPos(),whatsThis(),this);
}

void confInput::updateFontInfo()
{
	labelFont = QApplication::font();
	labelFont.setPointSize(labelFont.pointSize()-1);
	if(user_level == 1) 
		labelFont.setItalic(true);

	setFont(labelFont);
	label->setFont(labelFont);
	for(int i=0;i<lEdits.size();i++)
		lEdits[i]->setFont(labelFont);

	if(yesLabel != NULL) yesLabel->setFont(labelFont);
	if(noLabel != NULL) noLabel->setFont(labelFont);

	labelFont.setPointSize(labelFont.pointSize()-1);

	if(menu != NULL)
		menu->setFont(labelFont);
	update();
}

bool confInput::event(QEvent *event)
{
	if(event->type() == QEvent::WhatsThisClicked)
	{
		event->accept();
		QString whatsThisResult = ((QWhatsThisClickedEvent*)event)->href();
		if(whatsThisResult.contains("element://sync"))
		{
			QString value = element->get("sync_with_upper_level").trimmed().toLower();

			if(!value.isEmpty())
			{
				if(value == "yes") value = "no";
				else if(value == "no") value = "yes";
			}

			element->set("sync_with_upper_level",value);
			updateWhatsThis();
			dataModified();
			QWhatsThis::hideText();
		}
		else
			QProcess::startDetached(data->getApp("webBrowser") + " " + whatsThisResult);
		return true;
	}
	else
		return QWidget::event(event);
}

int confInput::userLevel()
{
	return user_level;
}

void confInput::dataModified()
{
	data->setModified(true);
}

void confInput::updateStatus()
{
	if(data->isModified())
	{
		/* 2dx_ATTENTION */
		confData *statusData = data;
		QString value = element->get("valuelabel").trimmed().toLower();
		if(value == "stepdigitizer" || value == "magnification" || value == "imagesidelength")
		{
			statusData->set("UNBENDING_done","n");
			statusData->set("DEFOCUS_done","n");
			statusData->set("CTF_done","n");
		}
		else if(value == "lattice")
		{
			statusData->set("LATTICE_done","y");
			statusData->set("SPOTS_done","n");
			statusData->set("UNBENDING_done","n");
			statusData->set("CTF_done","n");
		}
		else if(value == "realcell" || value == "realang")
		{
			statusData->set("UNBENDING_done","n");
			statusData->set("CTF_done","n");
		}
		else if(value == "defocus")
		{
			statusData->set("DEFOCUS_done","y");
			statusData->set("CTF_done","n");
		}
	}
}

void confInput::updateWhatsThis()
{
	QString whatsthis = element->get("valuelabel") + "<br><br>" + element->get("legend") + "<br><br>";
	bool minDefined=false, maxDefined = false;
	minDefined = element->get("type").contains("min",Qt::CaseInsensitive);
	maxDefined = element->get("type").contains("max",Qt::CaseInsensitive);
	if(minDefined || maxDefined)
	{
		QString type = element->get("type");
		type.replace(QRegExp("^.*\"(.*)\".*$"),"\\1");
		QStringList fields = type.split(';');
		QString minimum, maximum;
		QMap<int,QStringList> minMaxPairs;
		minDefined = false; maxDefined = false;
		bool currentMaxDefined, currentMinDefined;
		int k = 0;
		foreach(QString field, fields)
		{
			if(minDefined && field.contains("min", Qt::CaseInsensitive)) {k++; minDefined = false; maxDefined = false;}
			if(maxDefined && field.contains("max", Qt::CaseInsensitive)) {k++; minDefined = false; maxDefined = false;}
			currentMaxDefined = field.contains("max", Qt::CaseInsensitive);
			currentMinDefined = field.contains("min", Qt::CaseInsensitive);

			if(currentMinDefined) {minMaxPairs[k].insert(0,field.section('=',-1,-1).trimmed()); minDefined = true;}
			if(currentMaxDefined) {minMaxPairs[k].insert(1,field.section('=',-1,-1).trimmed()); maxDefined = true;}
		}

		QMapIterator<int,QStringList> it(minMaxPairs);

		whatsthis += "Range";
		if(minMaxPairs.size()>1) whatsthis +="s: <br>";
		else whatsthis+=": ";

		foreach(QStringList p, minMaxPairs)
			while(it.hasNext())
			{
				it.next();
				if(minMaxPairs.size()>1) whatsthis+=QString::number(it.key() + 1) + ": ";
				QStringList p = it.value();
				maxDefined = false; minDefined = false;
				if(p.size()>0) minDefined = !p[0].isEmpty();
				if(p.size()>1) maxDefined = !p[1].isEmpty();

				if(minDefined)
				{
					if(!maxDefined) whatsthis+=" Min=";
					whatsthis += p[0];
				}
				if(maxDefined)
				{
					if(!minDefined) whatsthis+="Max=";
					else whatsthis+=" to ";
					whatsthis+= p[1];
				}
				whatsthis += "<br>";
			}
		whatsthis += "<br>";
	}
	if(!element->get("sync_with_upper_level").isEmpty())
		whatsthis += tr("Sync with upper level: ") + "<a href=\"element://sync\">" + element->get("sync_with_upper_level").trimmed().toLower() + tr("</a>") + "<br><br>";
	whatsthis += "Example: " + element->get("example") + "<br><br>";
	whatsthis += "<a href=\"" + element->get("help") + "\"> " + element->get("help") + "</a>";
	setWhatsThis(whatsthis);

}

void confInput::show()
{
	for(int i=0;i<lEdits.size();i++)
	{
		lEdits[i]->show();
		//repaint();
    update();
		updateGeometry();
	}
	layout->invalidate();
	layout->update();
	update();
	QWidget::show();
	emit shown();
}
