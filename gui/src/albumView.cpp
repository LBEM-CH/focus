#include <albumView.h>

albumView::albumView(QWidget *parent)
         : QListView(parent)
{
  setViewMode(QListView::IconMode);
  setMovement(QListView::Snap);
}
