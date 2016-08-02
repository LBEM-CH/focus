
#ifndef MRCHEADERDISPLAY_H
#define MRCHEADERDISPLAY_H

#include <QLabel>
#include <QGridLayout>
#include <confData.h>
#include <mrcHeader.h>

class mrcHeaderDisplay : public QWidget
{
  private:
  confData *conf;
  QString result;
  QList<QLabel *> titles;
  QList<QLabel *> labels;

  QLabel *titleLabel(const QString &text, bool color);
  void setColors(int start, int end, bool color);

  public:
  mrcHeaderDisplay(confData *data, QWidget *parent = NULL);
  void setHeader(const QString &result, mrcHeader header);
};

#endif
