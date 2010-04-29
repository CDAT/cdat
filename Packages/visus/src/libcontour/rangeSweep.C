
// $Id: rangeSweep.C,v 1.1 2003/09/02 17:27:17 scorzell Exp $

#include <stdio.h>
#include "rangeSweep.h"
//#include "squeue.h"
#include "iqueue.h"

#define DEBUGNo
#define DEBUGSLEEPNo
#define MAXQUEUESIZE

class QueueRec {
   public:
      QueueRec(int c=0) { init(c); }

      void init(int c) { cellid=c;
#if 0
                         nfc=0;
#endif
                         range[0].MakeEmpty();
                         range[1].MakeEmpty();
                         range[2].MakeEmpty();
                         range[3].MakeEmpty();
                         range[4].MakeEmpty();
                         range[5].MakeEmpty();
                       }

      int operator ==(QueueRec &qr) { return(cellid == qr.cellid); }

#if 0
      int isFromCell(int c) { int i;
                              for (i=0; i<nfc; i++)
                                 if (fromcell[i] == c)
                                    return(1);
                              return(0);
                            }
      int addFromCell(int c) { fromcell[nfc++] = c; }
#endif

      int cellid;
#if 0
      int fromcell[6];
      int nfc;
#endif
      Range fullrange;
      Range range[6];
};

#ifdef MAXQUEUESIZE
static int maxqsize = 0;
#endif

void
rangeSweep::PropagateRegion(int cellid, float min, float max)
{
//   SQueue<QueueRec> q;
   static IndexedQueue<QueueRec, int> q;
   QueueRec qr, current, *old;
   RangeSweepRec rsr, *rsritem;
   int c, adjc, cindex;
//   int index;
//   int entry;

   qr.init(cellid);
   qr.fullrange.Set(min, max);
//   q.enqueue(qr);
   q.enqueue(qr, cellid);

//if (cellid == 777) {
//   sleep(5);
//}
   while (!q.isEmpty()) {
#ifdef MAXQUEUESIZE
if (q.getLength() > maxqsize) {
   maxqsize = q.getLength();
   if (maxqsize%10000 == 0)
      printf("qsize: %d\n", maxqsize);
}
#endif
      q.dequeue(current);
//      item = q.dequeue();
//      current = *item;

#ifdef DEBUG
printf("propagating from cell %d: ", current.cellid);
current.fullrange.Print();
printf("(%d %d %d) (%f %f %f)\n", data.getCellVert(current.cellid, 0),
                                  data.getCellVert(current.cellid, 1),
                                  data.getCellVert(current.cellid, 2),
                                  data.getValue(data.getCellVert(current.cellid, 0)),
                                  data.getValue(data.getCellVert(current.cellid, 1)),
                                  data.getValue(data.getCellVert(current.cellid, 2)));
#endif

      // if cell is done, remove from heap and continue;
      if (plot.CellTouched(current.cellid)) {
#ifdef DEBUG
printf("cell touched\n");
#endif
         rsr.cellid = current.cellid;
//         if ((entry=queue.find(rsr)) != -1)
//            queue.remove(entry);
         if (queue.find(rsr.cellid) != NULL)
            queue.remove(rsr.cellid);
         continue;
      }

      // if cell not in heap, add it
      rsr.cellid = current.cellid;
//      if ((entry=queue.find(rsr)) == -1) {
      if ((rsritem=queue.find(current.cellid)) == NULL) {
#ifdef DEBUG
printf("adding cell to heap\n");
#endif
         data.getCellRange(rsr.cellid, min, max);
         rsr.range.Set(min, max);
         queue.insert(rsr, max-min, rsr.cellid);
         rsritem=queue.find(rsr.cellid);
      }

//      rsritem = queue.nthEntry(entry);

#ifdef DEBUG
printf("heap item range: ");
rsritem->range.Print();
current.fullrange.Print();
#endif

      if (rsritem->range.Disjoint(current.fullrange)) {
#ifdef DEBUG
printf("ranges are disjoint\n");
#endif
         continue;
      }

      // subtract off the propagated range
      rsritem->range -= current.fullrange;

      // update priority, possibly removing item from queue
      if (rsritem->range.Empty()) {
#ifdef DEBUG
printf("cell is now empty, removing\n");
#endif
         queue.remove(current.cellid);
//         queue.remove(entry);
         plot.TouchCell(current.cellid);
      }
      else {
#ifdef DEBUG
printf("new priority is %f-%f\n", rsritem->range.MaxAll(), rsritem->range.MinAll());
#endif
         queue.updatePriority(current.cellid, rsritem->range.MaxAll()-
                                               rsritem->range.MinAll());
      }


      // don't use rsritem after this point.. may be deleted
      rsritem = NULL;


      // propagate ranges to shared faces
//      current = *item;
      for (c=0; c<data.getNCellFaces(); c++) {
         adjc = data.getCellAdj(current.cellid, c);

         if (adjc != -1 && !plot.CellTouched(adjc)) {
            // get the range of the shared face
            data.getFaceRange(current.cellid, c, min, max);
#ifdef DEBUG
printf("queuing cell %d (adj %d to %d)\n", adjc, c, current.cellid);
printf("range of face is %f %f\n", min, max);
#endif
#ifdef DEBUGSLEEP
   sleep(1);
#endif

            // find the index of this cell
            cindex = data.getAdjIndex(adjc, current.cellid);
if (cindex == -1) {
   printf("index -1!!\n");
#ifndef WIN32
   sleep(5);
#endif
}

            // propagate to this cell the intersection
            qr.init(adjc);
            qr.fullrange = current.fullrange;
            // need to take complement, but be careful not to remove
            // a constant cell, lest we not complete the propagation
            if (qr.fullrange.MinAll() != qr.fullrange.MaxAll()) {
               qr.fullrange -= Range(-10000000, min);
               qr.fullrange -= Range(max, 10000000);
            }
            // don't propagate anything which came from this face
            qr.fullrange -= current.range[c];
#ifdef DEBUG
printf("fullrange: ");
qr.fullrange.Print();
printf("face range: ");
current.range[c].Print();
#endif
//            if ((index = q.find(qr)) != -1) {
            if ((old = q.find(adjc)) != NULL) {
               // item already in queue
//               old=q.getItem(index);
               old->fullrange += qr.fullrange;
               old->range[cindex] += qr.fullrange;
//               if (!old->isFromCell(current.cellid))
//                  old->addFromCell(current.cellid);
            }
            else if (!qr.fullrange.Empty()) {
               qr.range[cindex] = qr.fullrange;
//               qr.addFromCell(current.cellid);
//               q.enqueue(qr);
               q.enqueue(qr, adjc);
            }
         }
         else {
#ifdef DEBUG
printf("cell %d (adj %d to %d) skipped, must be -1 or touched\n", adjc, c, current.cellid);
#endif
         }
      }
   }
}

void
rangeSweep::compSeeds(void)
{
   RangeSweepRec rsr, item;
   Range fullrange, added, outgoing;
   float min, max;
   //double p;

   printf("------- computing seeds\n");

   // clear the array of mark bits
   plot.ClearTouched();
   seeds.Clear();

   // insert cell 0 into queue to begin
   rsr.cellid = 0;
   data.getCellRange(0, min, max);
   rsr.range.Set(min, max);
//   queue.insert(rsr, max-min);
   queue.insert(rsr, max-min, rsr.cellid);

   // process queue of cells
   while (! queue.isEmpty() ) {
      // get the item
      //p = queue.max(item);
      queue.max(item);

      // cell is a seed cell
      seeds.AddSeed(item.cellid, item.range.MinAll(), item.range.MaxAll());

#ifdef DEBUG
printf("*\n*\n*\ncell %d is a seed\n*\n*\n*\n", item.cellid);
#endif
#ifdef DEBUGSLEEP
sleep(3);
#endif

      // mark this cell as processed
//      plot.TouchCell(item.cellid);

      PropagateRegion(item.cellid, item.range.MinAll(), item.range.MaxAll());
   }

   printf("computed %d seeds\n", seeds.getNCells());
}
