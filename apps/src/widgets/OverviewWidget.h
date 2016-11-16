#ifndef OVERVIEWWIDGET_H
#define OVERVIEWWIDGET_H

#include <QWidget>
#include <QStackedWidget>
#include <QLabel>
#include <QToolButton>
#include <QList>
#include <QTimer>

#include "ImageViewer.h"
#include "GraphicalButton.h"
#include "OverviewSettings.h"
#include "ParameterConfiguration.h"

class OverviewWidget : public QWidget {
public:

    OverviewWidget(QWidget* parent = 0);
    
    void setCurrentImagePath(const QString& imagePath);
    void setPreviewImages();
    void resetOverview();

    void activateOverlay(bool);
    void changeOverlaidWidget();
    
    void maximizePreview(ImageViewer* viewer, bool maximize);
    
private:
    
    QString parseVariables(const QString& string, ParametersConfiguration* conf);
    
    QString currentImagePath_;
    
    QStackedWidget* overlayWidgets_;

    GraphicalButton* rightButton_;
    GraphicalButton* leftButton_;
    
    QToolButton* showHeaderButton_;
    QToolButton* overlayButton_;
    QTimer overlayTimer_;
    
    QList<ImageViewer*> previewList_;
    QList<QWidget*> previewTitleWidgets_;
    QList<QLabel*> previewLabels_;
    QList<OverviewSettings::OverviewProps> properties_;
    QLabel* overviewLabel_;
    int overviewIndex_ = 0;
    QWidget* previewGridWidget_;
};

#endif /* OVERVIEWWIDGET_H */

