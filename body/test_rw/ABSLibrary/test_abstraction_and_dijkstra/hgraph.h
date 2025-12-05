/*********************************************** (c) Marcus von Lossow, 2006
  GR, 2008-

  MODULE : hgraph.h
  PURPOSE: Hypergraphenstruktur
  CHANGE : 2009


***************************************************************************/

#ifndef __HGRAPH_H
#define __HGRAPH_H

#ifdef TEST
#include <assert.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

typedef struct HEdgeStructure
{
  double Weight;                   /* Gewicht der Hyperkante */
  struct HVertexStructure *start;  /* Startknoten */
  struct HVertexStructure ** ziel; /* Array von Zeigern, auf   */
  int NoOfEdges;                   /* Anzahl der Kanten in der Hyperkante */
  int label;                       /* label of h-edge, kann z.B. ein Steuersignal bezeichnen.
		                      H-Kanten, die von verschiedenen Knoten ausgehen, koennen 
	                              durchaus gleiche labels haben */
} HEdge;

typedef struct HVertexStructure
{
  double wert;       /* Wert der Funktion V aus Dijkstra-Algorithmus */
  int NoOfInHEdges;  /* Anzahl der Hyperkanten, die auf diesen Knoten zeigen */
  HEdge** InHEdges;  /* Array von Zeigern auf diese Hyperkanten */
  HEdge *Nachfolger; /* Zeiger auf Kante, die kuerzesten Pfad realisiert */
  int posh;          /* Position in der Halde (bei Dijkstra-Algorithmus) */
  int status;        /* Status 0: Knoten gehoert zu \skript Q
  		        Status 1: Knoten gehoert nicht zu \skript Q */
} HVertex;

typedef struct HGraphStructure
{
  int NoOfHVertices;  /* Anzahl der Knoten */
  int NoOfHEdges;     /* Anzahl der Hyperkanten */
  HVertex* HVertices; /* Knoten-Array */
  HEdge ** HEdges;    /* Hyperkanten-Array */
  char *name;         /* Name des Graphen oder des Problems */
} HGraph;

HGraph * neuer_graph(int anz);
/* Legt einen neuen Hypergraphen mit anz Knoten an (ohne Hyperkanten) */

HEdge * neue_kante(HVertex* start, double Weight); /* Legt eine neue Hyperkante mit Gewicht Weight an. */

void fuege_hkante_ein(HGraph* graph, HEdge * hypkante);
/* Fuegt eine Hyperkante in den Hypergraphen ein */



#endif
