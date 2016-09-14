#ifndef SELECTIONFFT_H
#define SELECTIONFFT_H

#include <QLabel>
#include <QGridLayout>
#include <QPixmap>

class SelectionFFT : public QWidget {
    Q_OBJECT
    
public:
    SelectionFFT(QWidget *parent = NULL);
    void fft(QWidget *widget, const QRect &view);
    void setBrightness(float brightness);
    void setZoom(float zoom);

public slots:
    void increaseZoom();
    void decreaseZoom();
    void zoomStandard();

private:
    QLabel *display;
    QPixmap image;

    float intensity;
    float scale;

    void calculateFFT();
};

#endif
