/*********************************************** (c) Marcus von Lossow, 2006-07


  MODULE : hgraph.c
  PURPOSE: Hypergraphenstruktur
  CHANGE : 18.7.2006


***************************************************************************/

#include "hgraph.h"
#include "errorcodes.h"

void tausche_h(int a, int b, HVertex **Halde); /*tauscht die Elemente a und b in Halde*/

HGraph * neuer_graph(int anz)
/* GR: hgraph(anz) liefert Zeiger auf Hypergraphen mit anz Knoten und 0 Kanten */
{
  int i;
  HGraph * graph;

  graph=(HGraph *)(malloc(sizeof(HGraph)));
  if (graph==NULL) parse_error(NOMEM);

  graph->NoOfHVertices = anz;
  graph->NoOfHEdges = 0;

  graph->HVertices=(HVertex *)(calloc(anz,sizeof(HVertex)));
  if (graph->HVertices==NULL) parse_error(NOMEM);

  for(i=0;i<anz;i++)
     {
       graph->HVertices[i].NoOfInHEdges=0;
       graph->HVertices[i].InHEdges=NULL;
       graph->HVertices[i].Nachfolger=NULL;
     }
  graph->HEdges = NULL;
  return graph;
}

HEdge * neue_kante(HVertex* start, double Weight)
{
  HEdge* kante;

  kante=(HEdge *)(malloc(sizeof(HEdge)));
  if (kante==NULL) parse_error(NOMEM);

  kante->Weight = Weight;
  kante->start = start;
  kante->NoOfEdges=0;
  kante->ziel= NULL;

  return kante;
}


void fuege_hkante_ein(HGraph* graph, HEdge * hypkante)
{
  int i;
  graph->NoOfHEdges++;
 
  /* hypkante an H-Kanten-Feld von graph anhaengen (shit): */
  graph->HEdges=(HEdge**)(realloc(graph->HEdges,graph->NoOfHEdges*sizeof(HEdge*)));
  if (graph->HEdges==NULL) parse_error(NOMEM);

  graph->HEdges[graph->NoOfHEdges - 1] = hypkante;

  /* Fuer alle Zielknoten (== alle Kanten) von hypkante: aktualisiere Information (shit) */
  for(i=0;i<hypkante->NoOfEdges;i++)
     {
        hypkante->ziel[i]->NoOfInHEdges++;

	hypkante->ziel[i]->InHEdges=(HEdge**)(realloc(hypkante->ziel[i]->InHEdges,(hypkante->ziel[i]->NoOfInHEdges)*sizeof(HEdge*)));
	if (hypkante->ziel[i]->InHEdges==NULL) parse_error(NOMEM);

	hypkante->ziel[i]->InHEdges[hypkante->ziel[i]->NoOfInHEdges-1]=hypkante;
     }
  return;
}

void tausche_h(int a, int b, HVertex **Halde)
{
  HVertex * temp;
  temp=Halde[a];
  Halde[a]=Halde[b];
  Halde[b]=temp;
  Halde[a]->posh=a;
  Halde[b]->posh=b;
}
