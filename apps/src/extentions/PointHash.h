#ifndef POINTHASH_H
#define POINTHASH_H

inline uint qHash(const QPoint &key)
{
  return qHash(key.x()) ^ key.y();
}

inline bool operator<(const QPoint &a, const QPoint &b)
{
  if(a.x() == b.x()) return a.y()<b.y();
  return a.x()<b.x();
}

#endif
