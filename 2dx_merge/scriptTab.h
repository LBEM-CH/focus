/* 
 * File:   scrpitTab.h
 * Author: biyanin
 *
 * Created on April 30, 2015, 9:37 AM
 */

#ifndef SCRIPTTAB_H
#define	SCRIPTTAB_H

#include <QWidget>
#include <QStackedWidget>

#include <scriptModule.h>
#include <resizeableStackedWidget.h>
#include <confInterface.h>
#include <confManual.h>

class scriptTab : public QWidget
{
    Q_OBJECT

    public slots:
    
        void loadParameters();
        void updateFontInfo();
    
    public:
        
        scriptTab(scriptModule* mod, confData* mainData, QWidget *parent = NULL);

        void setManualIndex(int index);
        int addManualWidget(confManual* man);
        void hideManual();
        void showManual();

        void setParameterIndex(int index);
        int addParameterWidget(confInterface* interf);
        void hideLocalParameters();
        void showLocalParameters();

        void selectPrameters(const QStringList &selectionList);
        
        scriptModule* getModule();

    
    private:

        scriptModule* module;
        resizeableStackedWidget *localParameters;
        confInterface *parameters;
        QStackedWidget *manual;
    
};

#endif	/* SCRIPTTAB_H */

