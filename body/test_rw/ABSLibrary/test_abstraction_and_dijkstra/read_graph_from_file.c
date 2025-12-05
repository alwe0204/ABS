/* Schalter: */
/* #define TEST */ /* falls gesetzt: assert u.a. zulassen */
#define TIMER /* falls gesetzt: Zeitmessung */
/* -------------------- End Schalter -------------------- */

/*
Aufruf: main

Eingabe: stdin
Ausgabe: stdout

Formate der Ein- und Ausgabe
============================

Eingabe: Hypergraph (mit Marken fuer die Hyperkanten), Menge der
	 Zielknoten

Ausgabe: Fuer jeden Knoten mit endlichem Abstand zur Zielknotenmenge
	 wird dieser Abstand angegeben zusammen mit der Marke einer
	 Hyperkante, die den Abstand realisiert.

Input-Format:
=============
1. Kommentarzeichen:='c'
2. Jede mit einem Kommentarzeichen beginnende Zeile ist eine
   Kommentarzeile. Diese Zeilen duerfen nicht laenger als MAXLINE
   Zeichen sein und werden ignoriert. (MAXLINE ist in main.c
   definiert.) Wenn im folgenden von "Zeilen" gesprochen wird, sind
   immer Nicht-Kommentar-Zeilen gemeint.
   EINSCHRAENKUNG DER IMPLEMENTATION: Nach Angabe der Zielknoten duerfen keine Kommentarzeilen folgen; zwischen den Feldern duerfen keine Kommentarzeilen sein.

3. Die erste Zeile hat die Form:
HGRAPH

4. Die 2. Zeile hat die Form:
<Anzahl Knoten im Graphen> <Anzahl Hyperkanten> <Anzahl Kanten insg.>
Die Anzahl der Knoten musz positiv sein.

4. Die 3. Zeile hat die Form:
TARGET

5. Die 4. Zeile hat die Form:
<Zielknoten 1> <Zielknoten 2> ...
Es musz mindestens ein Zielknoten vorhanden sein.

6. Die 5. Zeile hat die Form:
HEDGES
EINSCHRAENKUNG DER IMPLEMENTATION: Diese Zeile Weglssen


7. Die weiteren Zeilen haben die Form:
<Startknoten> <Marke der H-Kante> <Gewicht der HKante>
<Anz. Zielknoten> <Zielkn 1> <Zielknoten 2> ...
<Zielknoten "Anz Zielknoten">

EINSCHRAENKUNG DER IMPLEMENTATION: Hyperkanten muessen in aufsteigender Reihenfolge stehen, mit 1 beginnend.
-----------------------------------------------------------------------
* Knotenmenge: 1,...,<Anz Knoten in G>

Output-Format:
==============
1. Kommentarzeichen:='c'
2. Jede mit einem Kommentarzeichen beginnende Zeile ist eine
   Kommentarzeile. Diese Zeilen duerfen nicht laenger als MAXLINE
   Zeichen sein und werden ignoriert. (MAXLINE ist in main.c
   definiert.) Wenn im folgenden von "Zeilen" gesprochen wird, sind
   immer Nicht-Kommentar-Zeilen gemeint.
3. Fuer jeden Knoten, der Abstand < \infty von Zielknotenmenge hat, eine Zeile der Form:
<Knoten> <Abstand> <Marke einer HKante, die Abstand realisiert>


*/

#ifdef TEST
#include	<assert.h>
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<malloc.h>
#include	<string.h>

#ifdef TIMER
#include        "timer.h"
#endif

#include        "hgraph.h"
#include        "errorcodes.h"
#include <unistd.h>
#define MAXLINE 8192    /* max. Zeilenlaenge in Eingabe */
#define COMMENT_CHAR 'c'
#define PATH_TO_GRAPH "../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/Abstraction.dat"
#define PATH_TO_CONTROLLER "../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/MController.dat"

  HGraph* graph;            /* Hypergraph */
  HVertex** TargetNodes;         /* Feld von Zeigern auf Zielknoten */
int NumOfTargetNodes; /* Anzahl Zielknoten */
int err;
int NumOfHVertices, NumOfHEdges, NumOfEdges;
int Lastusedindex;
int NumOfControls;
int first_control;
int NumOfnonTargetNodes;
int* array_for_faster_search;
float*array_of_values;

int *array_of_succ;

int *array_of_initial_nodes;
float* values;
int** array_of_controls;
int **controls;
int *num_of_controls,*num_of_controls2;
int *array_of_initial_nodes2;
FILE *f;
int i;
int k=0;
int k2=0;
int k_temp=0;



int correctness_of_controller(int index, int c)
/*
  param: index - cell index, c - control input
  return 0 if the control input c for the given cell index
   belongs to the maximum controller
  return 1 otherwise

 */
{
 for(int j=0; j<num_of_controls[index-1];j++) if(array_of_controls[index-1][j]==c) return 0;
return 1;
}


void free_hgraph(void)
{

  for(int k=0;k<NumOfnonTargetNodes;k++)free(array_of_controls[k]);

  free(array_of_controls);
  free(graph->HEdges);
  free(graph->HVertices);
  free(TargetNodes);
  free(array_for_faster_search);
  free(array_of_succ);
  free(array_of_initial_nodes);
  free(values);
  free(controls);
  free(array_of_values);
  free(array_of_initial_nodes2);


}

int get_num_of_control_symbols(void)
/*
   return: number of control symbols
*/
{

  for (int i=0;i<NumOfHEdges-1;i++)
  if(graph->HEdges[i]->start!=graph->HEdges[i+1]->start)return graph->HEdges[i]->label+1;

  return 0;
}


int get_first_control_symbol(void)
{

return graph->HEdges[0]->label;


}




int numb_of_succ(int cell,int v)
{
  /*
    param:cell - cell index, v - control input
    return: number of successor for the pair cell , v

   */

  if( k_temp==0)
    {
      array_for_faster_search=(int *)malloc(NumOfHVertices * sizeof(int));
      for(int j=0;j<NumOfHVertices;j++)array_for_faster_search[j]=-1;

      for(int j=0;j<NumOfHEdges;j++)


	 if(array_for_faster_search[graph->HEdges[j]->start-(graph->HVertices)]==-1)
	  {
	    array_for_faster_search[graph->HEdges[j]->start-(graph->HVertices)]=j;

	  }

    k_temp=1;
    }



  k=array_for_faster_search[cell];

  if(k!=-1)
{
if(graph->HEdges[k]->start-(graph->HVertices)==cell&&graph->HEdges[k]->label==v) {return graph->HEdges[k]->NoOfEdges;}


 while(k<NumOfHEdges-1 &&  graph->HEdges[k]->start-(graph->HVertices)<=cell)
      {
	if(graph->HEdges[k]->start-(graph->HVertices)==cell&&graph->HEdges[k]->label==v)

       {return graph->HEdges[k]->NoOfEdges;}

	k+=1;
    }
}
 return 0;

}

int* create_array_of_succ(int cell,int v)
/*

  param: cell - cell index, v- control input
  return: array of successors

*/
{

  int numb_of_succ;

  k2=array_for_faster_search[cell];

  if(k2!=-1)
    {
    if(graph->HEdges[k2]->start-(graph->HVertices)==cell&&graph->HEdges[k2]->label==v)
	{
	  numb_of_succ=(graph->HEdges[k2]->NoOfEdges);

	  int i;

	  array_of_succ=(int *)realloc(array_of_succ,numb_of_succ * sizeof(int));

	  for (i = numb_of_succ-1;  i>=0; i--)
	    {
	      array_of_succ[i]=graph->HEdges[k2]->ziel[i]-graph->HVertices;

	    }

	  return array_of_succ;

	}


   while(k2<NumOfHEdges-1 && (graph->HEdges[k2]->start-(graph->HVertices)<=cell ))
      {
k2+=1;
      if(graph->HEdges[k2]->start-(graph->HVertices)==cell&&graph->HEdges[k2]->label==v)
	{
	  numb_of_succ=(graph->HEdges[k2]->NoOfEdges);

	  int i;

	      array_of_succ=(int *)realloc(array_of_succ,numb_of_succ * sizeof(int));

	  for (i = numb_of_succ-1;  i>=0; i--)
	    {
	      array_of_succ[i]=graph->HEdges[k2]->ziel[i]-graph->HVertices;

	    }

	  return array_of_succ;

	}

      }
}
 return NULL;

}




int* create_array_of_target_cells(void)
/*

  return: array of target cells

*/
{

  int *array_of_target_nodes,i;

  array_of_target_nodes=(int *)malloc(NumOfTargetNodes*sizeof(int));;

  for (i = 0; i < NumOfTargetNodes; i++)
    {
      array_of_target_nodes[i]=TargetNodes[i]-(graph->HVertices)+1;

    }

 return array_of_target_nodes;

}



int* create_array_of_initial_cells(void)
/*


  return: array of initial cells

  Opens file Controller.dat

  In addition to the array of initial cells
  reads array of cell values and maximum controller
  from the file

*/
{

  int c;
  float value;
  values=(float *)malloc((NumOfHVertices)*sizeof(float));
  array_of_initial_nodes=(int *)malloc((NumOfHVertices)*sizeof(int));
  int control,number;


  num_of_controls2  =(int *)malloc((NumOfHVertices)*sizeof(int));
  controls=(int**)malloc((NumOfHVertices)*sizeof(int*));

  char buffer[MAXLINE];

  FILE*file=fopen(PATH_TO_CONTROLLER,"r");

  if (file) {
  //skip first 2 lines of file

   fgets(buffer, MAXLINE, file);
   fgets(buffer, MAXLINE, file);

  i=0;
       do
      {
	fscanf(file, "%d", &c);
	fscanf(file, "%f", &value);
	fscanf(file, "%d", &number);

	num_of_controls2[i]=number;
	controls[i]=(int *)malloc((number)*sizeof(int));
	for (int j=0;j<number;j++)
	  {
	    fscanf(file, "%d", &control);
	    if(control>=0) controls[i][j]=control;

	  }

	if(value>=0.0) values[i]=value;


	if(c>=0) {array_of_initial_nodes[i]=c;i++;}

      } while (fgets(buffer, MAXLINE, file) != NULL);

    fclose(file);


    array_of_initial_nodes2=(int *)malloc((i-1)*sizeof(int));
    array_of_values=(float *)malloc((i-1)*sizeof(float));
    array_of_controls=(int **)malloc((i-1)*sizeof(int*));
    num_of_controls  =(int *)malloc((i-1)*sizeof(int));
    for(int j=0;j<i-1;j++)
      {
	array_of_initial_nodes2[j]=array_of_initial_nodes[j];
	array_of_values[j]=values[j];
	array_of_controls[j]=controls[j];
        num_of_controls[j]=num_of_controls2[j];
      }

    NumOfnonTargetNodes=i-1;

 return array_of_initial_nodes2;
  }

 return NULL;

}

#define HGRAPH_KEYW "HGRAPH"
#define TARGET_KEYW "TARGET"
#define HEDGES_KEYW "HEDGES"
#define NAMEEE_KEYW "NAMEEE"


#define max(a,b) (((a) < (b)) ? (b) : (a))

char *read_line_into_buffer(char buffer[MAXLINE]) {
  /*  static const char *THIS_FCT = "read_line_into_buffer";*/

  if (fgets(buffer, MAXLINE, stdin) == NULL)
    parse_error(BAD_LINE);

 if (buffer[ strlen(buffer) - 1 ] != '\n')
   parse_error(LINE_TOO_LONG);

  return buffer;
}

void skip_comment_lines(char buffer[MAXLINE]) {
  /* ueberspringe alle Zeilen, die mit COMMENT_CHAR beginnen */
  /*  static const char *THIS_FCT = "skip_comment_lines";*/
  int z;
//printf("star skip");
   do {
   z = getc(stdin);

  //  printf("skip_comment_lines: char read: %c\n", (char)z);
    ungetc(z,stdin);

   }  while ( ( (char)z == COMMENT_CHAR ) && read_line_into_buffer(buffer) );
//printf("done");
}

int parse_hgraph (void) {
  /*
    liest Hypergraph und Zielknotenfeld von stdin;
Out:
    graph: Pointer auf gelesenen Graphen,
    V2: Pointer auf Zielknotenfeld
    return: 0 gdw fehlerfrei, sonst error index
   */

  if( access("../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/Abstraction.dat", F_OK ) == -1 ||
      access("../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/MController.dat", F_OK ) == -1 )
    if( access("../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/Abstraction_and_MController.zip", F_OK ) != -1 )
   {
	printf("Necessary files with abstract problem missing\n");
	printf("Attempting to extract archive\n");
     system("unzip -o ../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/Abstraction_and_MController.zip -d ../../../body/test_rw/ABSLibrary/test_abstraction_and_dijkstra/ ");
  /*Unzips archive if Abstraction.dat or MController.dat is missing*/
      }
  if(
 freopen(PATH_TO_GRAPH, "r", stdin)!=NULL)
{
  char buffer[MAXLINE], /* buffer fuer z.B. Kommentare */
    *name, /* fuer Namen des Problems bzw des Graphen */
    format_string[100],
    keyw[(sizeof(HGRAPH_KEYW)-1)/(sizeof(" ")-1) +1]; /* zum Einlesen der Worte: "HGRAPH", "TARGET", HEDGES" */



#ifdef TEST
  static const char *THIS_FCT = "parse_hgraph";

  assert((sizeof(HGRAPH_KEYW)==sizeof(TARGET_KEYW)) &&
	 (sizeof(HGRAPH_KEYW)==sizeof(HEDGES_KEYW)) &&
	 (sizeof(HGRAPH_KEYW)==sizeof(NAMEEE_KEYW)));
  printf("%s: Eintritt ---------------------------------------------------------------\n\n",THIS_FCT);
#endif

  /* lies NAMEEE_KEYW: ---------------------------------------- */

  skip_comment_lines(buffer);

  read_line_into_buffer(buffer);


  /* erstelle formatstring: */
  sprintf(format_string, "%%%lds", ( sizeof(NAMEEE_KEYW) - 1 ) / (sizeof(" ") - 1) + 1);

  //  printf("FORMATSTRING:%s:END\n", format_string);

  if ((sscanf(buffer,format_string, keyw) != 1) ||
      (strcmp(keyw,NAMEEE_KEYW) != 0)
      ) {
    fprintf(stderr,"Line expected to contain keyword %s starts with:\n%.10s\n", NAMEEE_KEYW, buffer);
    parse_error(BAD_KEYWORD_LINE);
    }

  /* lies name: */
  skip_comment_lines(buffer);

  read_line_into_buffer(buffer);

  name = (char *) malloc((sizeof(" ")-1) * MAXLINE);
  if(name == NULL ) parse_error(NOMEM);

  if ((sscanf(buffer,"%256s", name) != 1)) {
    fprintf(stderr,"Line expected to contain name of problem starts with:\n%.10s\n", buffer);
    parse_error(BAD_NAME);
  }

  /* lies HGRAPH_KEYW: ---------------------------------------- */
  skip_comment_lines(buffer);
  read_line_into_buffer(buffer);

  /* erstelle formatstring: */
  sprintf(format_string, "%%%lds", ( sizeof(HGRAPH_KEYW) - 1 ) / (sizeof(" ") - 1) + 1);

  /*  printf("FORMATSTRING:%s:END\n", format_string);*/

  if ((sscanf(buffer,format_string, keyw) != 1) ||
      (strcmp(keyw,HGRAPH_KEYW) != 0)
      ) {
    fprintf(stderr,"Line expected to contain keyword %s starts with:\n%.10s\n", HGRAPH_KEYW, buffer);
    parse_error(BAD_KEYWORD_LINE);
  }
//----------------------------------------------------------------------------------------------------------------------------
  /* lies NumOfHVertices, NumOfHEdges, NumOfEdges: ---------------------------------------- */
  skip_comment_lines(buffer);
  read_line_into_buffer(buffer);

  if ((sscanf(buffer,"%d%d%d", &NumOfHVertices, &NumOfHEdges, &NumOfEdges) != 3) ||
      (NumOfHVertices <= 0) ||
      (NumOfHEdges < 0) ||
      (NumOfEdges < NumOfHEdges)
      ) {
    fprintf(stderr,"Line expected to contain number of vertices, h-edges, and edges starts with:\n%.40s\n", buffer);
    parse_error(BAD_HGRAPH_DESC_LINE);
  }

   // printf("read: NumOfHVertices, NumOfHEdges, NumOfEdges:: %d, %d, %d\n", NumOfHVertices, NumOfHEdges, NumOfEdges);

  /* create graph: ---------------------------------------- */
  if ((graph = neuer_graph(NumOfHVertices))==NULL) parse_error(NOMEM);

  (graph)->name = name;
  /*  output_hgraph(*graph);*/

  /* lies TARGET_KEYW: ---------------------------------------- */
  skip_comment_lines(buffer);
  read_line_into_buffer(buffer);

  /* erstelle formatstring: */
  sprintf(format_string, "%%%lds", ( sizeof(TARGET_KEYW) - 1 ) / (sizeof(" ") - 1) + 1);

  /*  printf("FORMATSTRING:%s:END\n", format_string);*/

  if ((sscanf(buffer,format_string, keyw) != 1) ||
      (strcmp(keyw,TARGET_KEYW) != 0)
      ) {
    /* free mem graph */
    fprintf(stderr,"Line expected to contain keyword %s starts with:\n%.10s\n", TARGET_KEYW, buffer);
    parse_error(BAD_KEYWORD_LINE);
  }

  /* lies target set: ---------------------------------------- */
  skip_comment_lines(buffer);

  /* lies NumOfTargetNodes: */
  if ((scanf("%d", &NumOfTargetNodes) != 1)||
      (NumOfTargetNodes < 1) ||
      (NumOfTargetNodes > NumOfHVertices)
      ) {
    /* free mem graph */
    parse_error(BAD_NO_TARGET_NODES);
  }


  /* richte Feld fuer Nummern der target nodes ein: */
  TargetNodes = (HVertex**)(malloc(sizeof(HVertex*) * (NumOfTargetNodes)));

  if(TargetNodes==NULL) parse_error(NOMEM); /* free graph */

/*
  NummernDerTargetNodes = (int*) (malloc(sizeof(int) * NumOfTargetNodes));
  if(NummernDerTargetNodes==NULL) parse_error(NOMEM); */ /* free graph */

  /* lies Nummern der <NumOfTargetNodes> target nodes: */
  {
    int num;
    HVertex **ipt = TargetNodes + (NumOfTargetNodes); /* zeigt auf eines nach Ende des Feldes */

    while (--ipt >= TargetNodes) {
      if((scanf("%d",&num) != 1) ||
	 (num < 0) ||
	 (num > NumOfHVertices-1)
	 )
	parse_error(BAD_TARGET_NODE);
      *ipt = ((graph)->HVertices) + num-1 ;
    }


  }

/* lies HEDGES_KEYW: ---------------------------------------- */
/* erstelle formatstring: */
  sprintf(format_string, "%%%lds", ( sizeof(HEDGES_KEYW) - 1 ) / (sizeof(" ") - 1) + 1);

  /*  printf("FORMATSTRING:%s:END\n", format_string);*/

  if ((scanf(format_string, keyw) != 1) ||
      (strcmp(keyw,HEDGES_KEYW) != 0)
      ) {
    /* free mem graph */
    fprintf(stderr,"Line expected to contain keyword %s starts with:\n%.10s\n", HEDGES_KEYW, buffer);
    parse_error(BAD_KEYWORD_LINE);
  }


  /* lies Hyperkanten: ---------------------------------------- */
  {
    int NumDerAktuellenHKante, AkkumulierteAnzahlDerKanten = 0;



    for (NumDerAktuellenHKante=1; NumDerAktuellenHKante <= NumOfHEdges; NumDerAktuellenHKante++) {
      int num, label, AnzahlZielknotenInHKante;
      HEdge* HKante;
      HVertex **ipt, *StartKnoten;
      double Weight;

      /* lies Startknoten der HKante: */
      if ((scanf("%d",&num) != 1) ||
	  (num < 0 ) ||
	  (num > NumOfHVertices-1)
	  ) {
	fprintf(stderr, "Expected h-edge %d; number not found or incorrect.\n", NumDerAktuellenHKante);
	printf("%d",num);
	 parse_error(BAD_HEDGE_NUM);
      }
      StartKnoten = ((graph)->HVertices) + num+1 - 1;



      /* lies Label und Weight: */
      if ((scanf("%d%lf",&label,&Weight)!=2) ||
	  (label < 0) ||
	  (Weight < 0.)
	  ) {
	fprintf(stderr, "Expected label and weight of h-edge %d.\n", NumDerAktuellenHKante);
	parse_error(BAD_HEDGE_LABEL_OR_WEIGHT);
      }



      /* lies Anzahl Zielknoten in H-Kante: */
      if ((scanf("%d",&AnzahlZielknotenInHKante)!=1) ||
	  (AnzahlZielknotenInHKante < 0) ||
	  (AnzahlZielknotenInHKante > NumOfHVertices) ||
	  ((AkkumulierteAnzahlDerKanten += AnzahlZielknotenInHKante) > NumOfEdges)
	  ) {
	fprintf(stderr, "Expected valid number of target nodes in h-edge %d.\n", NumDerAktuellenHKante);
	parse_error(BAD_HEDGE_NUM_OF_TARGETS);
      }

      /* lege H-Kante an, setze Gewicht, Label, Anzahl Zielknoten; lege Feld fuer Zielknoten an: */
      HKante = neue_kante(StartKnoten, Weight);
      HKante->label = label;
      HKante->NoOfEdges = AnzahlZielknotenInHKante;
      HKante->ziel = (HVertex**)malloc(sizeof(HVertex*) * (HKante->NoOfEdges));
      if((HKante->ziel) == NULL ) parse_error(NOMEM);

      /* lies Zielknoten ein und schreibe in H-Kante: */
      ipt = (HKante->ziel) + (HKante->NoOfEdges); /* zeigt auf eines nach Ende des Feldes */
      while (--ipt >= (HKante->ziel)) {
      if((scanf("%d",&num) != 1) ||
	 (num < 0) ||
	 (num > NumOfHVertices-1)
	 )
	parse_error(BAD_HEDGE_TARGET);
      *ipt = ((graph)->HVertices) + num+1 - 1;
      }



      fuege_hkante_ein(graph, HKante);
      /*      output_hgraph(*graph);*/



    }

    if(AkkumulierteAnzahlDerKanten != NumOfEdges) {
      fprintf(stderr, "Number of h-edges is %d, but we have found only %d.\n", NumOfEdges, AkkumulierteAnzahlDerKanten);
      parse_error(TOO_FEW_HEDGES_READ);
    }

  }


  NumOfControls=get_num_of_control_symbols();
  Lastusedindex=NumOfHVertices-1;

  fclose(stdin);

      return 0;
    }
  return 1;
 }

#undef MAXLINE
#undef max
