#ifndef IMPORTTOOL_H
#define IMPORTTOOL_H
#include <QDialog>
#include <QLineEdit>
#include <QRegExp>
#include <QFile>
#include <QFileInfo>
#include <QDebug>
#include <ui_importTool.h>
#include <confData.h>

class importTool : public QDialog
{
  Q_OBJECT
  
  public slots:
  void setRegExp(const QString &regExp);
  void addPattern();
  void removePattern();
  void updateParsedView();
  void updateParsedView(const QString &regExp);
  void updateParsedViewRequired();
  void updateParsedViewRequired(int i, int j);
  bool savePatternList();
  bool savePatternList(const QString &path);
  bool loadPatternList();
  bool loadPatternList(const QString &path);
  void show();

  void accept();

  signals:
  void acceptedImages(const QHash<QString, QHash<QString,QString> > & imageList);
  
  private:
  Ui::ImportTool ui;
  confData *data;
  QStringList fileList;
  QStringList nameList;
  QStringList parsedHeaderLabels;
  QRegExp pattern;
  
  QStringList patternFileHeader;

  QHash<QString, QHash<QString, QString> > imageList;

  void exportParsedList();

  public:
  importTool(confData *data, const QStringList &importList, QWidget *parent = NULL);
};

#endif
