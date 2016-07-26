/*
 *  confInterface.h
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

 #ifndef CONFINTERFACE_H
 #define CONFINTERFACE_H

 #include <QWidget>
 #include <QLabel>
 #include <QGridLayout>
 #include <QVBoxLayout>
 #include <QHBoxLayout>
 #include <QLineEdit>
 #include <QString>
 #include <QAction>
 #include <QSet>
 #include <confData.h>
 #include <confSectionHeader.h>
 #include <confInput.h>
 #include <graphicalButton.h>

class confInterface : public QWidget
{
  Q_OBJECT

  public slots:
  void save();
  void load();
  void execute();
  void setUserLevel(int level);
  void setSelectionUserLevel(int level);
  void reselect(bool value = true);
  void updateFontInfo();
  void hideAll();

  void openAllSections();

  signals:
  void saveAll();
  void loadAll();
  void fontInfoUpdated();

  private:
  //Actions
  QAction *saveAction;
  QAction *loadAction;
  QAction *executeAction;
  confData *data;
  bool advancedView;
  QMultiMap<QWidget*,confInput*> lookup;
  QSet<QWidget*> emptyingSections;
  QList<confSectionHeader*> titles;
  QList<QWidget*> sectionHeaders;
  QHash<QString, confInput*> inputs;
  QStringList selectedInputs;

  int userLevel;
  int isWrong;

  QColor alt;

  public:
  confInterface(confData *conf, QString concerns = "", QWidget *parent = NULL);
  void select(const QStringList &selectionList, int level = 0);
  
};

#endif
