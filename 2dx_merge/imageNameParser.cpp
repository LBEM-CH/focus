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

#include <imageNameParser.h>


QHash<QString,QString> imageNameParser::parse(const QString &fileName)
{
  QHash<QString,QString> elements;
  QString file = QFileInfo(fileName).fileName();

  QRegExp fReg("(\\D+)(\\d+)(?:\\.)(mrc|tif?)");
  fReg.indexIn(file);
  
  QString pA = fReg.cap(1);
  QString integers = fReg.cap(2);
  QString ext = fReg.cap(3);
  
  elements.insert("file_name",fileName);
  elements.insert("extension",ext);
 
  if(integers.size()>=10)
  {
    QRegExp iReg("(\\d*)(\\d{2})(\\d{6})(\\d{2})$");
    iReg.indexIn(integers);

		elements.insert("protein_code",pA+iReg.cap(1));
    elements.insert("tilt_angle",iReg.cap(2));
    elements.insert("frame_number",iReg.cap(3));
    elements.insert("sub_image_number",iReg.cap(4));
  }
  
  return elements;
}
