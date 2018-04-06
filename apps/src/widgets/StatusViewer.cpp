#include <QtWidgets>

#include "StatusViewer.h"

StatusViewer::StatusViewer(ProjectImage* image, QWidget* parent)
: QWidget(parent) {
    setFixedHeight(150);
    timer_.setSingleShot(true);
    image_ = image;
    watcher_.setFile(image->cfgFile());
    
    connect(&watcher_, SIGNAL(fileChanged(const QString &)), this, SLOT(timedLoad()));
    connect(&timer_, SIGNAL(timeout()), this, SLOT(load()));
    
    setLayout(initializeLayout());
}

QHBoxLayout* StatusViewer::initializeLayout() {
    QHBoxLayout* mainLayout = new QHBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addStretch(0);
    
    //Initialize the mag and qval fields
    magLabel_ = new QLabel;
    qvalLabel_ = new QLabel;
    
    QFormLayout* rtlayout = new QFormLayout();
    rtlayout->setMargin(0);
    rtlayout->addRow("Calculated Mag: ", magLabel_);
    rtlayout->addRow("Last QVAL: ", qvalLabel_);
    
    phaseResLabel_ = new QLabel;
    numSpotsLabel_ = new QLabel;
    
    QFormLayout* ltlayout = new QFormLayout();
    ltlayout->setMargin(0);
    ltlayout->addRow("Sym. PhaRes: ", phaseResLabel_);
    ltlayout->addRow("# of spots: ", numSpotsLabel_);
    
    QHBoxLayout* layout = new QHBoxLayout();
    layout->setSpacing(5);
    layout->setMargin(0);
    layout->addLayout(rtlayout);
    layout->addLayout(ltlayout);
    
    //Initialize the bins table
    binsTable_ = prepareTable(2, 6);
    binsTable_->horizontalHeader()->hide();
    binsTable_->setVerticalHeaderLabels(QStringList() << "Resolution [A]" << "#");
    
    //Initialize the qval table
    qvalTable_ = prepareTable(4, 7);

    QStringList labels;
    labels << tr("IQ1") << tr("IQ2") << tr("IQ3") << tr("IQ4") << tr("IQ5") << tr("IQ6") << tr("QVAL");
    qvalTable_->setHorizontalHeaderLabels(labels);
    qvalTable_->setVerticalHeaderLabels(QStringList() << "Unbend I" << "Unbend II" << "Movie A" << "Movie B");
    
    //Initialize the tilt table
    tiltTable_ = prepareTable(4, 4);
    tiltTable_->setVerticalHeaderLabels(QStringList() << tr("Defocus") << tr("Lattice") << tr("Spot Split") << tr("Merge"));
    tiltTable_->setHorizontalHeaderLabels(QStringList() << "Grid TAxis" << "Grid TAngle" << "Xtal TAxis" << "Xtal TAngle");
    
    //Arrange everything in a layout
    QVBoxLayout* firstColLayout = new QVBoxLayout();
    firstColLayout->setMargin(0);
    firstColLayout->setSpacing(2);
    firstColLayout->addLayout(layout);
    firstColLayout->addStretch(1);
    firstColLayout->addWidget(new QLabel("Power Bins:"));
    firstColLayout->addWidget(binsTable_);
    QWidget* firstCol = new QWidget;
    firstCol->setLayout(firstColLayout);
    
    mainLayout->addWidget(firstCol, 1);
    mainLayout->addWidget(qvalTable_, 1);
    mainLayout->addWidget(tiltTable_, 1);
    
    return mainLayout;
}


void StatusViewer::load() {
    //Load the data
    if(image_) {
        
        ParametersConfiguration* conf = image_->parameters();
        magLabel_->setText(conf->getRoundedValue("CALCULATEDMAG", 2));
        qvalLabel_->setText(conf->getRoundedValue("QVAL", 2));
        phaseResLabel_->setText(conf->getValue("PHARES_SYM"));
        numSpotsLabel_->setText(conf->getValue("PHARES_NUM_SPOTS"));
        
        loadTable(binsTable_, conf, QStringList() << "RB" << "RP", QStringList() << "1" << "2" << "3" << "4" << "5" << "6", "_");
        loadTable(qvalTable_, conf, QStringList() << "U1" << "U2" << "UMA" << "UMB", 
                QStringList() << "IQ1" << "IQ2" << "IQ3" << "IQ4" << "IQ5" << "IQ6", "_");
        loadTable(tiltTable_, conf, QStringList() << "DEFOCUS" << "LATTICE" << "TTREFINE" << "MERGE",
                QStringList() << "TLTAXIS" << "TLTANG" << "TAXA" << "TANGL", "_", true);
        
        //Manually get the QVALs (as they are not in the format row_col)
        qvalTable_->item(0, 6)->setText(conf->getRoundedValue("QVAL1", 2));
        qvalTable_->item(1, 6)->setText(conf->getRoundedValue("QVAL2", 2));
        qvalTable_->item(2, 6)->setText(conf->getRoundedValue("QVALMA", 2));
        qvalTable_->item(3, 6)->setText(conf->getRoundedValue("QVALMB", 2));
        
    }
    
}

void StatusViewer::loadTable(QTableWidget* table, ParametersConfiguration* conf, 
        const QStringList& rows, const QStringList& cols, const QString& sep, bool roundDigits) {
    for(int row=0; row < rows.size(); ++row) {
        for(int col=0; col < cols.size(); ++col) {
            QString param = rows[row] + sep + cols[col];
            QString value;
            if(roundDigits) value = conf->getRoundedValue(param, 2);
            else value = conf->getValue(param);
            table->item(row, col)->setText(value);
        }
    }

    //table->resizeColumnsToContents();
}

QTableWidget* StatusViewer::prepareTable(int rows, int cols) {
    QTableWidget* table = new QTableWidget(this);
    table->setColumnCount(cols);
    table->setRowCount(rows);
    table->setSelectionBehavior(QAbstractItemView::SelectRows);
    table->setAttribute(Qt::WA_MacShowFocusRect, 0);
    table->setShowGrid(false);
    table->setAlternatingRowColors(true);
    table->setSortingEnabled(false);

    for (int c = 0; c < cols; ++c) {
        table->horizontalHeader()->setSectionResizeMode(c, QHeaderView::Stretch);
    }
    
    for(int row=0; row<rows; ++row) {
        for(int col=0; col<cols; ++col){
            QTableWidgetItem* tableItem = new QTableWidgetItem();
            tableItem->setFlags(tableItem->flags() ^ Qt::ItemIsEditable);
            tableItem->setTextAlignment(Qt::AlignCenter);
            // tableItem->setFont(QFont("Courier", 9, QFont::Bold));
            tableItem->setText("--");
            table->setItem(row, col, tableItem);
        }
        table->setRowHeight(row,26);
    }
    
    return table;
}


void StatusViewer::timedLoad() {
    timer_.start(100);
}

