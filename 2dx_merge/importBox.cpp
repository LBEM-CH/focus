/***************************************************************************
*   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
*   HStahlberg@ucdavis.edu                                                *
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This program is distributed in the hope that it will be useful,       *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*   GNU General Public License for more details.                          *
*                                                                         *
*   You should have received a copy of the GNU General Public License     *
*   along with this program; if not, write to the                         *
*   Free Software Foundation, Inc.,                                       *
*   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
***************************************************************************/
#include <importBox.h>

importBox::importBox(const QString &imageName, QWidget *parent, Qt::WindowFlags f)
          :QDialog(parent)
{
  setModal(true);
  setAttribute(Qt::WA_DeleteOnClose);  

  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);
  ui.setupUi(this);

  imageCodesList = imageNameParser::parse(imageName);
  
  ui.importFileNameBox->setText(imageName);
  ui.proteinAcronymBox->setText(imageCodesList["protein_code"]);
  ui.tiltAngleBox->setValue(imageCodesList["tilt_angle"].toUInt()/5 * 5);
  ui.frameNumberBox->setText(imageCodesList["frame_number"]);
  ui.subImageBox->setText(imageCodesList["sub_image_number"]);

 // if(!(imageCodesList["protein_code"].isEmpty() || imageCodesList["tilt_angle"].isEmpty() || imageCodesList["frame_number"].isEmpty() || imageCodesList["sub_image_number"].isEmpty()))
 //   accept();
 // else 
    show(); 
}

const QHash<QString, QString> importBox::imageCodes()
{
  return imageCodesList;
}

void importBox::accept()
{
  acceptImage();
  QDialog::accept();
}

void importBox::acceptImage()
{
  imageCodesList["protein_code"] = ui.proteinAcronymBox->text();
  imageCodesList["tilt_angle"] = QString("%1").arg(ui.tiltAngleBox->value(),2,10,QChar('0'));
  imageCodesList["frame_number"] = ui.frameNumberBox->text();
  imageCodesList["sub_image_number"] = ui.subImageBox->text();
  emit acceptedImage(imageCodesList);
}

void importBox::hide()
{
  close();
}
