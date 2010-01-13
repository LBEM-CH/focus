#include <albumView.h>

albumView::albumView(QObject *parent)
         : QListView(parent)
{
  setViewMode(QListView::IconMode);
  setMovement(QListView::Snap);
}
