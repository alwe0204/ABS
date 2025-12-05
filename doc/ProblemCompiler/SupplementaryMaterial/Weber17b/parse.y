// \section{The programming language}
// \label{s:main}
// \subsection{Purpose and formal definition}
// \label{ss:purpose}
// The purpose of the actual programming language 
// to be specified is to represent functions in the set 
//   \begin{align}
//  \nonumber F = \{ f \colon X \to Y \mid& 
// \exists_{n \in \mathbb{N}}\exists_{d_1,\ldots,d_n \in \mathbb{N}} \ \emptyset \neq X \subseteq  \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n} 
//    \text{ and}\\ & \exists_{m \in \mathbb{N}} \exists_{(k_1,l_1),\ldots,(k_m,l_m) \in \mathbb{N} \times \mathbb{N} } \ 
// Y = \mathbb{R}^{k_1 \times l_1} \times \ldots \times \mathbb{R}^{k_m \times l_m} \}. \label{e:F}
//   \end{align}
// %(Here and subsequently, the involved form of domains and target sets is intensionally, of course.)
// To be more precise, a function in $F$ that will be representable in the programming language 
// must have a domain defined through intervals and be either explicitly expressible through a formula 
// or implicitly 
// as a function defined through a solution of an ordinary differential equation.
// The first case is further restricted to functions
// whose components are finite compositions of elementary operations 
// and some other functions such as sine and exponentials.
// To discuss the second case, 
// consider a function
// \begin{equation}
// \label{e:f}
// f \colon X \times U \to \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n},
// \end{equation}
// where $\emptyset \neq X \subseteq \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}$, 
// $\emptyset \neq U \subseteq \mathbb{R}^{d_{n+1}} \times \ldots \times \mathbb{R}^{d_{n+m}}$,
// $n,m,d_1,\ldots,d_{n+m} \in \mathbb{N}$, $f(\cdot,u)$ is extendable to some neighborhood $\tilde X$ of $X$
// such that the extension\footnote{$\tilde X$ may depend on $u \in U$. The particular extension does not play a part in the sequel.} 
// is of class $C^\infty$ on $\tilde X$, 
// and consider the ordinary differential equation
// \begin{align}
// \label{e:ode}
// D_1 x &= f(x,u).
// \end{align}
// Here and subsequently, $D_1 g$ denotes the vector 
// whose components are the derivatives of the components of $g$ with respect to the first argument. 
// %We also identify $\mathbb{R}^k$ with $\mathbb{R}^{k \times 1}$, $k \in \mathbb{N}$.
// \par
// Before proceeding, we would like to mention that 
// the motivation for considering \ref{e:ode} with $f$ of the involved form \ref{e:f} are dynamical systems
// that possess various kinds of states such as observable and internal states or whose dynamics depend on different kinds of inputs
// such as control inputs and parameters. E.g., we may write \ref{e:ode} in case $n=m=2$ as $D_1(x,y)=f((x,y),(u,v))$ if we wish to indicate
// the different kinds of $x,y$ and $u,v$, and a program will allow for a corresponding representation for $f$ in this case.\par
// A \begriff{solution of \ref{e:ode} on $(I,X_0,U_0)$} is a function
// 
// \begin{equation}
// \label{e:solution}
// \varphi \colon I \times X_0 \times U_0 \to \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}
// \end{equation}
// such that $\{0\} \subsetneq I \subseteq \mathbb{R}$, $I$ is a compact interval,
// $\emptyset \neq X_0 \subseteq X$, 
// $\emptyset \neq U_0 \subseteq U$, 
// $\im \varphi \subseteq X$, 
// $D_1\varphi(t,x,u) = f(\varphi(t,x,u),u)$ and $\varphi(0,x,u) = x$ for all $(t,x,u) \in \dom \varphi$. 
// We are going to define 
// the programming language such that 
// for a solution $\varphi$ of \ref{e:ode} on $(I,X_0,U_0)$ functions in $F$ that are of the form 
// \begin{equation}
// \label{e:psi}
// (t,y_1,\ldots,y_k,u_1,\ldots,u_m) \mapsto \big (\varphi_i(t,g(y_1,\ldots,y_k),(u_1,\ldots,u_m))\big )_{i \in J}
// \end{equation}
// are representable by a program. 
// Here, 
// $t \in I$, $(u_1,\ldots,u_m) \in U_0$, 
// $\emptyset \neq J \subseteq \intcc{1;n}$ and 
// $g \in F$ is such that $\im g \subseteq X_0$.
// More precisely, the programming language will allow to represent \ref{e:psi} by representing
// \ref{e:ode}, $g$ and the sets $I$,
// $U_0$, $J$.
// The arguments of \ref{e:psi} may also take any order, 
// so not necessarily the one in the left hand side of \ref{e:psi}.
// Whether \ref{e:psi} is representable will clearly depend on 
// whether $f$, $g$, $I$, $U_0$ can be represented.
//
// In particular, $f$, $g$ shall be given by an explicit formula.\footnote{$f \notin F$ if $n>1$ or $m>1$ 
// but we may easily identify $f$
// with a function in $F$ by flattening the domain of $f$.} 
// We will also allow to represent $f$ through a function $\bar f$
// that may additionally take an exceptional value $\zeta \notin \mathbb{R}$ and is such 
// that the restrictions of $f$ and $\bar f$ to $\im \varphi \times U_0$ coincide.
// 
// \par
// Analogously to \ref{e:ode}, we also consider ordinary differential equations
// \begin{align}
// \label{e:ode:2}
// D_1 x &= f(x),
// \end{align}
// where $f \colon X \to \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}$, 
// $\emptyset \neq X \subseteq \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}$, 
// $n,d_1, \ldots , d_n \in \mathbb{N}$ and 
// $f$ is extendable to some neighborhood of $X$ such that the extension is of class $C^\infty$.
// The notion of solution is adopted accordingly in this case. 
// See \ref{fig:1} and \ref{fig:2} for programs of the programming language.%
// \input{1_PoleSystem.tex}%
// \input{1_NeherJacksonNedialkov.tex}%
// %\input{2_PoleSystem.tex}
// We note that when a program represents a function defined through a solution 
// of an ordinary differential equation, 
// then it particularly contains a separate
// representation of the right hand side.
//
// \par
// Formally, the actual programming language is defined as
// \begin{equation}
// \label{e:programminglanguage}
// (F,G,C,\mathcal{T},\mathcal{I}_\texttt{prog},\zeta)
// \end{equation}
// with $F$ as above, $\zeta \notin \mathbb{R}\cup F$, and we follow the scheme 
// outlined in Section \ref{s:Introduction} to fully 
// specify \ref{e:programminglanguage}. 
// However, as most ingredients of \ref{e:programminglanguage} 
// will be considerably more complex than the respective ingredients for the 
// example programming languages in Section \ref{s:Introduction}, we will subsequently visually group 
// a Backus-Naur form and objects related to the form. We also emphasize that
// the use of forward references to definitions of nonterminals is conceptually unavoidable.
// \par
// The remaining specifications for \ref{e:programminglanguage} are organized as follows.
// $C$ will be specified in Section \ref{s:graphicalcharacters} 
// and $G$ in Section \ref{s:syntax}. 
// $\mathcal{I}_\texttt{prog}$ is specified in Section \ref{ss:startsymbol}, where
// the set of states is specified in Section \ref{ss:S}.
// $\mathcal{T}$ is specified in Section \ref{s:tokenizer} as well as the keywords.
// \subsection{Set of valid graphical characters}
// \label{s:graphicalcharacters}
// The set $C$ in \ref{e:programminglanguage} is formed by the letters of the Latin alphabet 
//   \verb|a|-\verb|z|, \verb|A|-\verb|Z|, the digits 
//   \verb|0|-\verb|9|, the graphical characters
//   \begin{equation*}
//   \verb|;| \   \ \verb|:| \   \ \verb|.| \  \ \verb|,| \   \ \verb|+| \   \ \verb|-| \   \ \verb|*| \   \ \verb|/| \   \
//   \verb|^| \   \ \verb|=| \  \ \verb|[| \   \ \verb|]| \   \ \verb|(| \   \ \verb|)| \   \
//   \verb|{| \   \ \verb|}| \   \ \verb|_|
//   \end{equation*}
// and the whitespace, tab and new line characters. 
// \subsection{Syntax and Denotational Semantics}
// \label{s:syntax}
// In this section, $G$ and $\mathcal{I}_\mathrm{prog}$ in \ref{e:programminglanguage} are specified. 
// We note that the whitespace, tab and new line characters are not defined as terminals of $G$, i.e.,
// they do not appear in any Backus-Naur form below. 
// Instead, the tokenizer $\mathcal{T}$ handles these characters appropriately.
// \subsubsection{Latin alphabet and digits}
// \label{ss:latin}
// \begin{production}
//\begin{verbatim}
// NONDIGIT
//	: 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' 
//	| 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' 
//	| 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' 
//	| 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' 
//	| 'W' | 'X' | 'Y' | 'Z' | '_' 
//	;   
// \end{verbatim}
// \end{production}
// \begin{production}
// \begin{verbatim}
// DIGIT
//	: '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' 
//	;
// \end{verbatim}
// \begin{semantics}
// The standard interpretation function 
// $\mathcal{I} \colon [\verb|DIGIT|] \to \intcc{0;9}$ is used, 
// e.g., $\mathcal{I}\llbracket (\verb|1|)\rrbracket = 1$.
// \end{semantics}
// \end{production}
// %\begin{enumerate}
// %\item $\mathcal{I}\llbracket \verb|cos|\rrbracket = \cos$ (cosine function),
// %\item $\mathcal{I}\llbracket \verb|exp|\rrbracket = \exp$ (exponential function),
// %\item $\mathcal{I}\llbracket \verb|log|\rrbracket =  \ln$ (natural logarithm),
// %\item $\mathcal{I}\llbracket \verb|sin|\rrbracket = \sin$ (sine function),
// %\item $\mathcal{I}\llbracket \verb|sqrt|\rrbracket = (x \mapsto \sqrt{x}) $ (square root).
// %\end{enumerate}
// %$\mathcal{I}$ is to be considered in combination with \verb|call_function| (Section \ref{ss:expression}). Note also 
// %the extended domains of definition.
//\subsubsection{Nonnegative integers and decimal numbers}
// \label{ss:integers}
// \begin{production}
//\begin{verbatim}
// NNINTEGER
//	: DIGIT
//	| NNINTEGER DIGIT
//	;
//\end{verbatim}
// \begin{semantics}
// The standard interpretation function 
// $\mathcal{I} \colon [\verb|NNINTEGER|] \to \mathbb{Z}_+$
// as given in Section \ref{s:Introduction} is used. 
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
// DECIMAL
//	: NNINTEGER '.' NNINTEGER
//	| '.' NNINTEGER
//	| NNINTEGER '.'
//	| NNINTEGER '.' NNINTEGER 'E' '+' NNINTEGER
//	| NNINTEGER '.' NNINTEGER 'E' '-' NNINTEGER
//	| NNINTEGER '.' 'E' '+' NNINTEGER
//	| NNINTEGER '.' 'E' '-' NNINTEGER
//	| '.' NNINTEGER 'E' '+' NNINTEGER
//	| '.' NNINTEGER 'E' '-' NNINTEGER
//	| NNINTEGER '.' NNINTEGER 'e' '+' NNINTEGER
//	| NNINTEGER '.' NNINTEGER 'e' '-' NNINTEGER
//	| NNINTEGER '.' 'e' '+' NNINTEGER
//	| NNINTEGER '.' 'e' '-' NNINTEGER
//	| '.' NNINTEGER 'e' '+' NNINTEGER
//	| '.' NNINTEGER 'e' '-' NNINTEGER
//	| NNINTEGER '.' NNINTEGER 'E' NNINTEGER
//	| NNINTEGER '.' 'E' NNINTEGER
//	| '.' NNINTEGER 'E' NNINTEGER
//	| NNINTEGER '.' NNINTEGER 'e' NNINTEGER
//	| NNINTEGER '.' 'e' NNINTEGER
//	| '.' NNINTEGER 'e' NNINTEGER
//	;
//\end{verbatim}
// \begin{semantics}
// The standard interpretation function 
// $\mathcal{I} \colon [\verb|DECIMAL|] \to \mathbb{Q}_+$
// is used.
// \end{semantics}
// \end{production}
// \subsubsection{Identifiers}
// \label{ss:identifiers}
// \begin{production}
//\begin{verbatim}
// IDENTIFIER
//	: NONDIGIT
//	| IDENTIFIER NONDIGIT
//	| IDENTIFIER DIGIT
//	; 
//\end{verbatim}
// \end{production}
/****************************************************************
 *                                                              *
 *    End                                                       *
 *                                                              *
 ****************************************************************/
%{
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
#include <ctype.h>
extern char yytext[];
extern int yylex();
static void yyerror(char *);
%}

%token IDENTIFIER _PI NNINTEGER DECIMAL _ATAN _COS _COSH _EXP _LN _SIN _SINH _SQRT _TAN
%token REAL INTERVAL FUNCTION IN TO
%token FOR
%token DIFF COEFF INITIALVALUE
%token OPTION ORDER

%union { Expression expr ; Declaration decl ; Assignment def ; Statement stmt ; FunctionDefinition fct ; Iteration ite ; Ode ode ; Option opt ; struct ASTNODE * ast; }

%type <expr>  IDENTIFIER identifier identifier_list identifier_tuple 
%type <expr>  postfix_expression postfix_expression_list postfix_expression_tuple NNINTEGER DECIMAL _PI
%type <expr> expression_or_constant_interval_expression expression multiplicative_expression signed_expression exponential_expression primary_expression call_function constant_number number_or_variable continuous_interval_expression discrete_interval_expression constant_interval_expression nested_id_or_intv_expr_or_R_n_list nested_id_or_intv_expr_or_R_n_tuple id_or_intv_expr_or_R_n_tuple id_or_intv_expr_or_R_n_list id_or_intv_expr_or_R_n 
%type <decl> declaration declaration_and_definition
%type <def>  definition
%type <fct>  function_definition
%type <ite>  iteration_head 
%type <stmt> statement statement_list compound_statement iteration_statement initialvalue initialvalue_list
%type <ode> ode_definition ode_statement ode_equation
%type <opt> options option option_list option_head option_statement
%type <ast>  program
%start program
%%
/****************************************************************
 *                                                              *
 *  Yacc grammar specification                                  *
 *                                                              *
 ****************************************************************/
// \begin{production}
// \begin{verbatim}
identifier 
	: IDENTIFIER { $$ = current_id ; }
	;
// \end{verbatim}
// \noindent\textit{Remark:} This Backus-Naur form is required due to an implementation issue.
// \end{production}
// \subsubsection{Set of states}
// \label{ss:S}
// We now specify the set $S$ in \ref{e:states}.
// To begin with, 
// we fix a constant $n_{\mathrm{max}} \in \mathbb{N}$, define $\overline{\mathbb{R}} = \mathbb{R} \cup \{ \zeta \}$, 
// \begin{equation*}
// F_{\overline{\mathbb{R}}} = \{ f \colon X \to \overline{\mathbb{R}} \mid \exists_{n \in \mathbb{N}} \exists_{d_1,\ldots,d_n \in \mathbb{N}} \ \emptyset \neq X \subseteq \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n} \},
// \end{equation*}
// and denote by $I_\mathbb{R}$ the set of all closed 
// (continuous or discrete) intervals in $\mathbb{R}$. \\
// We set $A = [\verb|IDENTIFIER|]$ in \ref{e:states}
// and call the elements of $A$ alternatively \begriff{identifiers}.
// Before specifying $B$ in \ref{e:states}, 
// we motivate our choice of $B$ below.
// \\ 
// An identifier in the programming language should be capable of representing
// \begin{itemize}
// \item a real number or interval,
// \item a vector or matrix with real numbers, intervals or functions as entries,
// \item a function,
// \item an argument of a function, %(\begriff{bound variable} \cite[Sec.~3.2]{Tennent76}),
// \item the image of a function (output variable),
// \item the independent variable (time) in an ordinary differential equation,
// \item the dependent variable (state) in an ordinary differential equation.
// \end{itemize}
// Therefore, we will assign the following data to an identifier:
// \begin{enumerate}
// \item Two kinds of type, which are elements of the sets
// \begin{align*}
// \mathbf{T} =& \{ 
// \mathbf{Real},\mathbf{Interval},\mathbf{Function}, \mathbf{undeclared} \}, \text{ and } \\
// \mathbf{R} =& \{ 
// \mathbf{Ordinary}, \mathbf{Input},\mathbf{Output},
// \mathbf{Time},\mathbf{State},\mathbf{OutputState},\mathbf{InputConst} \},
// \end{align*}
// respectively;
// \item \label{l:ii} Two integers in $\intcc{0;n_\mathrm{max}}$ representing a row and column dimension;
// \item \label{l:iii} A matrix, where its dimensions are specified by the integers in \ref{l:ii}, and 
// an entry of the matrix may be one of the following objects:
// \begin{enumerate}
// \item
// A real number or $\zeta$, in both cases
// represented by a pair in $\overline{\mathbb{R}} \times I_\mathbb{R}$.
// We motivate this representation of real numbers in two situations.
// \\
// First, assume that we want to 
// represent some physical constant $g \in \mathbb{R}$ in the programming language, e.g.,
// the gravity at some position on earth. 
// $g$ might not be known exactly but we might be given some lower and upper bound
// $a,b \in \mathbb{R}$, respectively, on $g$,
// and some nominal value $g_0 \in \mathbb{R}$ for $g$. 
// Then we represent $g$ by
// $(g_0,\intcc{a,b})$,
// e.g., $(9.80665,\intcc{9.80,9.81})$
// is a representation of gravity. 
// \\
// For the second motivation, recall that 
// any programming language is finally implemented on a machine that
// only operates on floating-point numbers. 
// A real number $a \in \mathbb{R}$ that we may 
// represent by $(a,\intcc{a,a})$ has to be represented for an implementation by
// $(a_0,\intcc{a_-,a_+})$, where $a_0,a_-,a_+$ are floating-point numbers.
// Here, convenient choices of $a_0,a_-,a_+$ are such that $a \in \intcc{a_-,a_+}$ 
// and $a_0$ is a floating-point number with minimum distance to $a$. 
// For instance, the mathematical
// constant $\pi$ can be represented with double precision floating-point numbers by the choice
// \begin{alignat*}{3}
// &a_0 = a_- &&= \text{1.921fb54442d18}_{16} \cdot 2^1, \\
// & a_+ &&= \text{1.921fb54442d19}_{16} \cdot 2^1;
// \end{alignat*}
// %Finally, we want to point out that the three numerical values that represent
// %a real number have been introduced in the context of the intended use of the 
// %language, which is \emph{efficient} and \emph{formally correct} computations on floating-point arithmetic, 
// %and these quantities will be used for \emph{estimation} and 
// %\emph{validation}, respectively.
// %\\
// \item
// An interval, which is used for specifying a domain of a function;
// \item 
// A function $f \in F_{\overline{\mathbb{R}}}$. We use the extension 
// $\overline{\mathbb{R}}$ of $\mathbb{R}$ in order to let any 
// finite composition of functions be defined on $\mathbb{R}$. 
// E.g., $\mathbb{R} \to \overline{\mathbb{R}}$, 
// $x \mapsto 1/(1-x)$ with $1 \mapsto \zeta$ is a function in $F_{\overline{\mathbb{R}}}$.
// \end{enumerate} 
// An entry will be therefore an element of the set
// \begin{equation*}
// \mathbf{D} = ( \overline{\mathbb{R}}  \cup F_{\overline{\mathbb{R}}} ) \times I_\mathbb{R} .
// \end{equation*}
// \item An integer in $\intcc{0;n_\mathrm{max}}$ 
// to enumerate arguments of functions and output variables.
// \end{enumerate}
// So, we define $B$ as 
// \begin{equation*}
// B= (\mathbf{T} \times \mathbf{R} \times \intcc{0;n_\mathrm{max}} \times \intcc{0;n_\mathrm{max}} \times \mathbf{D}^{n_\mathrm{max} \times n_\mathrm{max} } \times \intcc{0;n_\mathrm{max}})
// \cup \{ \zeta \}
// \end{equation*}
// in \ref{e:states}, where we added $\zeta \in \overline{\mathbb{R}}$ to use it as an error symbol. \\
// We specify the initial state of the variables as
// the state $\sigma_0 \in S$
// such that
// \begin{equation}
// \label{e:initialstate}
// \sigma_0(x) = (\mathbf{undeclared},\mathbf{Ordinary},0,0,Z,0)
// \end{equation}
// for all $x \in A$, where $Z_{i,j} = (\zeta,\mathbb{R})$ for all $i,j \in \intcc{1;n_\mathrm{max}}$.
// \\ Subsequently, we write $\mathbf{error}$ for the state in $S$ that maps every $a \in A$ to $\zeta$. 
// Moreover, for an intuitive reference to components of $B$ we define 
// $$\mathrm{type} = 1, \ 
// \mathrm{role} = 2, \ 
// \mathrm{rowdim} = 3, \ \mathrm{coldim} = 4, \ \mathrm{data} = 5, \ \mathrm{idx} = 6.$$
// \subsubsection{Reading instructions}
// \label{ss:read}
// For the subsequent nonterminal $N_1 \defas \verb|postfix_expression|$ we define various auxiliary functions
// that we use to define other interpretation functions.
// \begin{production}
//\begin{verbatim}
postfix_expression
	: identifier  					                     	    { $$ = $1 ;                                 }
	| identifier '[' expression ']'		      	 /* vector */ { $$ = build_PostFixExpression($1,$3,NULL); }
	| identifier '[' expression ']' '[' expression ']' /* matrix */ { $$ = build_PostFixExpression($1,$3,$6);   }
	;
//\end{verbatim}
// \end{production}
// Before defining the auxiliary functions, we want to explain the use of the functions
// \[
// \operatorname{row}_0, \operatorname{col}_0, \operatorname{row}_1, \operatorname{col}_1
// \text{ and } 
// \operatorname{IsEntry},\] which are defined on $[N_1]$. 
// As we discussed in Section \ref{ss:S}, an identifier
// may represent a matrix with entries in $\mathbf{D}$. 
// Therefore, we have to introduce different
// Backus-Naur forms for 1) referring to an entry of the matrix and for 2) defining the dimensions of the matrix, i.e. the number of rows and columns. 
// For both operations we will make use of the nonterminal $N_1$ but the
// semantics of a word produced from $N_1$ 
// will depend on which of the previous two operations
// occurs. For instance, the word $a \defas \verb|X0[2]|$ in line 1 in \ref{fig:1} defines a dimension while the word
// $b \defas \verb|X0[0]|$ in line 2 refers to an entry. We use $\operatorname{row}_1$ and $\operatorname{col}_1$
// to interpret $\verb|X0|$ as a $2 \times 1$-matrix in line 1, we use $\operatorname{IsEntry}$ to check in line 2
// that $b$ is an entry of previous matrix, and $\operatorname{row}_0$ and $\operatorname{col}_0$ are used 
// to obtain the position of the entry in the matrix.
// Here, it is important to note that we adopt
// the index convention of the C programming language, i.e. indices start from $0$. 
// 
// We remark that if we replaced $a$ by $b$ in line 1 or $b$ by $a$ in line 2 then \ref{fig:1} would not
// be a program. 
// The reason in the first case is that dimensions of matrices shall be positive while in the second case
// $a$ would not refer to an entry of a matrix. These ``errors" would have been detected by
// the functions just introduced.
// \\
// Let $\mathcal{E}$ be the interpretation function for $\verb|expression|$ 
// and let $a \in [\verb|postfix_expression|]$. Then 
// $$a = x \ \text{ or } 
// \ a = x\verb|[|E\verb|]| \ \text{ or } 
// \ a = x\verb|[|E\verb|][|E'\verb|]|$$ 
// for some $x \in [\verb|identifier|]$, 
// $E,E' \in [\verb|expression|]$.\\ Define:
// \begin{enumerate}
// \item \label{e:name} $\mathrm{name} \colon [\verb|postfix_expression|] \to [\verb|identifier|]$ by $\mathrm{name}\segcc{a} = x $,
// \item 
// \begin{equation*}
// \label{e:rowcol}
//\mathrm{row}_\varepsilon,\mathrm{col}_\varepsilon \colon [\verb|postfix_expression|] \to \{ S \to \intcc{\varepsilon;n_\mathrm{max}} \cup \{ \zeta \} \}
// \end{equation*} with $\varepsilon \in \{0,1\}$, for all $\sigma \in S$ by
// \begin{align*}
// &\mathrm{row}_\varepsilon\segcc{x}(\sigma) = \mathrm{col}_\varepsilon\segcc{x}(\sigma) = \begin{cases} \zeta, & \text{if } \sigma = \mathbf{error} \\ \varepsilon, & \text{otherwise} \end{cases} , \\
// &\mathrm{col}_\varepsilon\llbracket x\texttt{[}E\texttt{]} \rrbracket(\sigma) = \mathrm{col}_\varepsilon\segcc{x}(\sigma),
// \end{align*}
// \begin{align*}
// &\mathrm{row}_\varepsilon\llbracket x\texttt{[}E\texttt{]} \rrbracket(\sigma) = \begin{cases} \zeta, & \text{if } \mathcal{E}\segcc{E}(\sigma) \notin  \intcc{\varepsilon;n_\mathrm{max}} \\ \mathcal{E}\segcc{E}(\sigma), & \text{otherwise} \end{cases} ,\\
// &\mathrm{row}_\varepsilon\llbracket x\texttt{[}E\texttt{]}\texttt{[}E'\texttt{]} \rrbracket(\sigma) = 
// \begin{cases} \zeta, & \text{if } 
// \mathcal{E}\segcc{E}(\sigma) \notin \intcc{\varepsilon;n_\mathrm{max}} \\
// \zeta, & \text{else if } \mathcal{E}\segcc{E'}(\sigma) \notin \intcc{\varepsilon;n_\mathrm{max}} \\
// \zeta, & \text{else if }
// \mathcal{E}\segcc{E}(\sigma)\cdot \mathcal{E}\segcc{E'}(\sigma)>n_\mathrm{max} \\ 
// \mathrm{row}_\varepsilon\llbracket x\texttt{[}E\texttt{]} \rrbracket(\sigma), & \text{otherwise}
// \end{cases}, \\
// &\mathrm{col}_\varepsilon\llbracket x\texttt{[}E\texttt{]}\texttt{[}E'\texttt{]} \rrbracket(\sigma) = 
// \begin{cases} \zeta, & \text{if } 
// \mathcal{E}\segcc{E}(\sigma) \notin \intcc{\varepsilon;n_\mathrm{max}} \\
// \zeta, & \text{else if } \mathcal{E}\segcc{E'}(\sigma) \notin \intcc{\varepsilon;n_\mathrm{max}} \\
// \zeta, & \text{else if } 
// \mathcal{E}\segcc{E}(\sigma)\cdot \mathcal{E}\segcc{E'}(\sigma)>n_\mathrm{max} \\
// \mathcal{E}\segcc{E'}(\sigma), & \text{otherwise} 
// \end{cases}.
// \end{align*}
// \item 
// \label{e:IsEntry} $\operatorname{IsEntry} \colon [\verb|postfix_expression|] \to \{ S \to \{0,1\} \}$ for all
// $\sigma \in S$ by 
// \begin{equation*}
// \operatorname{IsEntry}\segcc{x}(\sigma) = \begin{cases}
// 0, & \text{if } \sigma = \mathbf{error} \\
// 0, & \text{else if } \sigma(x)_\mathrm{type} \in \{\mathbf{undeclared},\mathbf{Function} \}  \\
// 0, & \text{else if } \sigma(x)_{\mathrm{rowdim}} \neq 1 \\
// 0, & \text{else if } \sigma(x)_{\mathrm{coldim}} \neq 1 \\
// 1, & \text{otherwise}
// \end{cases},
// \end{equation*}
// \begin{equation*}
// \operatorname{IsEntry}\llbracket x\verb|[|E\verb|]| \rrbracket(\sigma) = \begin{cases}
// 0, & \text{if } \sigma = \mathbf{error} \\
// 0, & \text{else if } \sigma(x)_\mathrm{type} \in \{\mathbf{undeclared},\mathbf{Function} \}  \\
// 0, & \text{else if } \mathrm{row}_0\llbracket x\verb|[|E\verb|]| \rrbracket(\sigma) \notin \intcc{0;\sigma(x)_{\mathrm{rowdim}}-1} \\
// 0, & \text{else if } \sigma(x)_{\mathrm{coldim}} \neq 1 \\
// 1, & \text{otherwise}
// \end{cases},
// \end{equation*}
// \begin{equation*}
// \operatorname{IsEntry}\llbracket x\verb|[|E\verb|]|\verb|[|E'\verb|]| \rrbracket(\sigma) = \begin{cases}
// 0, & \text{if } \sigma = \mathbf{error} \\
// 0, & \text{else if } \sigma(x)_\mathrm{type} \in \{\mathbf{undeclared},\mathbf{Function} \}  \\
// 0, & \text{else if } \mathrm{row}_0\llbracket x\verb|[|E\verb|]|\verb|[|E'\verb|]| \rrbracket(\sigma) \notin \intcc{0;\sigma(x)_{\mathrm{rowdim}}-1} \\
// 0, & \text{else if } \mathrm{col}_0\llbracket x\verb|[|E\verb|]|\verb|[|E'\verb|]| \rrbracket(\sigma) \notin \intcc{0;\sigma(x)_{\mathrm{coldim}}-1} \\
// 1, & \text{otherwise}
// \end{cases}.
// \end{equation*}
// \item
// Define 
// \begin{equation*}
// \operatorname{ReadEntry} \colon
// [ \verb|postfix_expression|] \to 
// \{ S \to \mathbf{D} \cup \{\zeta\} \}
// \end{equation*}
// for all $\sigma \in S$ by 
// \begin{equation*}
// \operatorname{ReadEntry}\segcc{a}(\sigma) = \begin{cases} 
// \zeta, & \text{if } \mathrm{IsEntry}\segcc{a}(\sigma) = 0   \\ 
// \big (\sigma( \mathrm{name}\segcc{a})_{\mathrm{data}} \big )_{r,c} & \text{otherwise}  \end{cases},
// \end{equation*}
// where $(r,c) = (1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma))$.
// \end{enumerate} 
// \subsubsection{Basic writing instructions}
// %The auxiliary functions used in this section are defined in Section \ref{ss:read}.
// \begin{production}
// \begin{verbatim}
declaration
	: REAL     postfix_expression ';' { $$ = install_declaration(TY_REAL,$2); }
	| INTERVAL postfix_expression ';' { $$ = install_declaration(TY_INTV,$2); }
	;
// \end{verbatim}
// \begin{semantics}
// Let $a \in  [\verb|postfix_expression|]$. Define 
// $$ \mathcal{I} \colon [\verb|declaration|] \to  \{ S \to S \}$$ for all $\sigma \in S$ by
// \begin{enumerate}
// \item
// \begin{equation*}
// \mathcal{I}\llbracket \verb|REAL |a\texttt{;} \rrbracket (\sigma) = 
// \begin{cases} 
// \mathbf{error}, & \text{if } \sigma = \mathbf{error} \\
// \mathbf{error}, & \text{else if } \sigma(\operatorname{name}\segcc{a})_{\text{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, & \text{else if } \zeta \in \{ \mathrm{row}_1\segcc{a}(\sigma) ,\mathrm{col}_1\segcc{a}(\sigma)\} \\
// \sigma ', & \text{otherwise}
// \end{cases} 
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{align*}
// & \sigma'(\operatorname{name}\segcc{a})_{\mathrm{type}} = \mathbf{Real}, \\
// & \sigma'(\operatorname{name}\segcc{a})_{\mathrm{rowdim}} = \mathrm{row}_1\segcc{a}(\sigma), \\
// & \sigma'(\operatorname{name}\segcc{a})_{\mathrm{coldim}} = \mathrm{col}_1\segcc{a}(\sigma).
// \end{align*}
// \item
// \begin{equation*}
// \mathcal{I}\llbracket \verb|INTERVAL |a\texttt{;} \rrbracket (\sigma) = 
// \begin{cases} 
// \mathbf{error}, & \text{if } \sigma = \mathbf{error} \\
// \mathbf{error}, & \text{else if } \sigma(\text{name}\segcc{a})_{\text{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, & \text{else if } \zeta \in \{ \mathrm{row}_1\segcc{a}(\sigma),\mathrm{col}_1\segcc{a}(\sigma)\} \\
// \sigma ', & \text{otherwise}
// \end{cases}  
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{align*}
// &\sigma'(\operatorname{name}\segcc{a})_{\text{type}} = \mathbf{Interval}, \\
// &\sigma'(\operatorname{name}\segcc{a})_{\mathrm{rowdim}} = \mathrm{row}_1\segcc{a}(\sigma), \\ 
// &\sigma'(\operatorname{name}\segcc{a})_{\mathrm{coldim}} = \mathrm{col}_1\segcc{a}(\sigma).
// \end{align*}
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
// %	| postfix_expression '='  constant_interval_expression               ';' { $$ = install_assignment(2,$1,$3); }
// \begin{verbatim}
definition  
	: postfix_expression '='  expression_or_constant_interval_expression ';' { $$ = install_assignment(1,$1,$3); }
	| postfix_expression  IN expression_or_constant_interval_expression ';'  { $$ = install_assignment(3,$1,$3); }
	;
// \end{verbatim}
// \begin{semantics}
// Let $a \in [\verb|postfix_expression|]$, $E \in [\verb|expression_or_constant_interval_expression|]$. 
// Let $\mathcal{I}_1$ and $\mathcal{I}_2$ be the interpretation functions
// for $\verb|expression|$ and \linebreak$\verb|constant_interval_expression|$.
// Define 
// \begin{equation*}
// \mathcal{I} \colon [\verb|definition|] \to \{ S \to S \}
// \end{equation*} 
// for all $\sigma \in S$ by
// \begin{enumerate}[(i)]
// \item
// \label{definition:i}
// \begin{equation*}
// \mathcal{I}\segcc{a \texttt{=} E\texttt{;}}(\sigma)= 
// \begin{cases} 
// \mathbf{error,} & \text{if } \operatorname{IsEntry}\segcc{a}(\sigma) = 0 \\
// \mathbf{error,} & \text{else if }  \sigma(\operatorname{name}\segcc{a})_\mathrm{role} =  \mathbf{Input} \\
// \mathbf{error,} & \text{else if } \sigma(\operatorname{name}\segcc{a})_\mathrm{type} \notin \{ \mathbf{Real},\mathbf{Interval} \} \\
// \mathbf{error,} & \text{else if } \sigma(\operatorname{name}\segcc{a})_\mathrm{type} = \mathbf{Real} \\
// &\text{and } \operatorname{IsNumber}\segcc{E}(\sigma) = 0  \\
// \mathbf{error,} & \text{else if } \sigma(\operatorname{name}\segcc{a})_\mathrm{type} = \mathbf{Interval} \\
// &\text{and } \operatorname{IsBoundedInterval}\segcc{E}(\sigma) = 0  \\
// \sigma_\mathrm{postfix}, & \text{else if } E \in [\verb|postfix_expression|] \\ 
// \sigma_\mathrm{Real}, & \text{else if } \sigma(\operatorname{name}\segcc{a})_\mathrm{type} = \mathbf{Real} \\
// \sigma_\mathrm{Interval}, & \text{otherwise} 
// \end{cases}
// \end{equation*}
// where $\sigma_\mathrm{postfix}$$, \sigma_\mathrm{Real}$, $\sigma_\mathrm{Interval}$ coincide with $\sigma$ except that 
// \begin{align}
// \big (\sigma_\mathrm{postfix}(\text{name}\segcc{a})_{\mathrm{data}}\big )_{1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma)} &= \operatorname{ReadEntry}\segcc{E}(\sigma),  \label{e:use:ReadEntry} \\
// \big ((\sigma_\mathrm{Real}(\text{name}\segcc{a})_{\mathrm{data}})_{1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma)}\big )_1 &= \mathcal{I}_1\segcc{E}(\sigma), \label{e:notuse:ReadEntry} \\
// \big ((\sigma_\mathrm{Interval}(\operatorname{name}\segcc{a})_{\mathrm{data}})_{1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma)}\big )_2 &= \mathcal{I}_2\segcc{E}(\sigma). \label{e:notuse:ReadEntry:2} 
// \end{align}
// \begin{remarks}
// Note that \ref{e:use:ReadEntry} defines \emph{both} components in $\mathbf{D}$ \emph{explicitly} 
// in contrast to \ref{e:notuse:ReadEntry},\ref{e:notuse:ReadEntry:2}. 
// \end{remarks}
// \item  
// \label{definition:iii}
// \begin{equation*}
// \small
// \mathcal{I}\llbracket a \verb| IN | E\texttt{;}\rrbracket (\sigma) = 
// \begin{cases}
// \mathbf{error,} & \text{if } \operatorname{IsEntry}\segcc{a}(\sigma) = 0 \\
// \mathbf{error,} & \text{else if }  \sigma(\operatorname{name}\segcc{a})_\mathrm{role} =  \mathbf{Input} \\
// \mathbf{error,} & \text{else if } \sigma(\operatorname{name}\segcc{a})_\mathrm{type} \neq \mathbf{Real} \\ 
// \mathbf{error,} & \text{else if } \operatorname{IsBoundedInterval}\segcc{E}(\sigma) = 0 \\
// \mathbf{error,} & \text{else if}\footnote{By this case, we require in a program to use \ref{definition:iii} for $a$, if at all, \emph{before} using \ref{definition:i} for $a$. } \ ((\sigma(\operatorname{name}\segcc{a})_{\mathrm{data}})_{1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma)})_1 \neq \zeta \\
// \sigma_\mathrm{postfix}, & \text{else if } E \in [\verb|postfix_expression|] \\ 
// \sigma_\mathrm{Interval}, & \text{otherwise} \end{cases},
// \end{equation*}
// where $\sigma_\mathrm{postfix}$, $\sigma_\mathrm{Interval}$ coincide with $\sigma$ except that 
// \begin{align*}
// &(\sigma_\mathrm{postfix}(\operatorname{name}\segcc{a})_{\mathrm{data}})_{1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma)} = 
// (c_1,( \operatorname{ReadEntry}\segcc{E}(\sigma))_2), \\
// &(\sigma_\mathrm{Interval}(\operatorname{name}\segcc{a})_{\mathrm{data}})_{1+\mathrm{row}_0\segcc{a}(\sigma),1+\mathrm{col}_0\segcc{a}(\sigma)} = 
// (c_2,\mathcal{I}_2\segcc{E}(\sigma))
// \end{align*}
//  with $c_1$ and $c_2$ being the midpoint of $( \operatorname{ReadEntry}\segcc{E}(\sigma))_2$ and $\mathcal{I}_2\segcc{E}(\sigma)$, respectively.
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
declaration_and_definition
	: REAL      identifier '='  expression                                 ';' { $$ = install_declaration_and_definition(TY_REAL,$2,$4); }
	| INTERVAL  identifier '='  expression_or_constant_interval_expression ';' { $$ = install_declaration_and_definition(TY_INTV,$2,$4); }
	| REAL      identifier  IN expression_or_constant_interval_expression ';'  { $$ = install_declaration_and_definition(TY_REAL,$2,$4); }
	;
//\end{verbatim}
// \begin{semantics}
// The interpretation function $\mathcal{I} \colon [\verb|declaration_and_definition|] \to \{ S \to S \}$ 
// is easily obtained from the obvious composition of 
// the interpretation functions for $\verb|declaration|$ and
// $\verb|definition|$.
// \end{semantics}
// \end{production}
// \subsubsection{Sequences of basic writing instructions}
// \begin{production}
// \begin{verbatim}
statement
	: declaration                { $$ = install_statement(DECL,(void *)$1)     ; }
	| definition                 { $$ = install_statement(DEFI,(void *)$1)     ; }
	| declaration_and_definition { $$ = install_statement(DECLDEFI,(void *)$1) ; }
	| iteration_statement        { $$ = $1                                     ; }
	;
// \end{verbatim}
// \begin{semantics}
// The interpretation function $\mathcal{I} \colon [\verb|statement|] \to \{ S \to S \}$ is defined through the 
// interpretation function for the respective alternative.
// \end{semantics}
// \end{production}
// \begin{production}
// \begin{verbatim}
statement_list
	: statement                { $$ = $1 ;                                    } 
	| statement_list statement { $$ = (Statement)append(AST_STMT,(void *)$1,(void *)$2); }
	;
// \end{verbatim}
// \begin{semantics}
// Let $a \in [\verb|statement_list|]$, $b \in [\verb|statement|]$ and
// $\mathcal{I}_1$ be the interpretation function for $\verb|statement|$. 
// Define $$\mathcal{I} \colon [\verb|statement_list|] \to \{ S \to S \}$$
// for all $\sigma \in S$ by 
// $\mathcal{I}\segcc{b} = \mathcal{I}_1\segcc{b}$ and 
// $\mathcal{I}\segcc{a \ b}(\sigma) = \mathcal{I}_1\segcc{b}(\sigma')$, 
// where $\sigma ' = \mathcal{I}\segcc{a}(\sigma)$.
// \end{semantics}
// \end{production}
// \begin{production}
// \begin{verbatim}
compound_statement 
	: '{' statement_list '}' { $$ = $2 ; }
	;
// \end{verbatim}
// \begin{semantics}
// Let $a \in [\verb|statement_list|]$ and $\mathcal{I}_1$ 
// be the interpretation function for $\verb|statement_list|$.
// Then define $$\mathcal{I} \colon [\verb|compound_statement|] \to \{ S \to S \}$$ by
// $\mathcal{I}\llbracket \verb|{| a \verb|}| \rrbracket = \mathcal{I}_1\segcc{a}$.
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
iteration_head
	: FOR '(' identifier '=' expression ',' expression ',' '+' ')'         { $$ = install_iteration(IT_FOR_INC,$3,$5,$7) ; }
	| FOR '(' identifier '=' expression ',' expression ',' '-' ')'         { $$ = install_iteration(IT_FOR_DEC,$3,$5,$7) ; }
	;
//\end{verbatim}
// \begin{aux}
// Loosely speaking, the function $\operatorname{iter}$ extracts the iteration control variable, the lower and upper iteration bounds and the 
// direction of the iteration from a word produced from $\verb|iteration_head|$. $\operatorname{iter}$ will be used in combination with $\verb|iteration_statement|$.\\
// Let $\mathcal{E}$ be the interpretation function for
// $\verb|expression|$, and $E,E' \in [\verb|expression|]$, $x \in [\verb|identifier|]$.  
// Define $$\operatorname{iter} \colon [\verb|iteration_head|] \to \big \{ S \to ([\verb|identifier|] \times \mathbb{Z}_+ \times \mathbb{Z}_+ \times \{-1,0,1\}) \cup \{ \zeta \} \big \}$$
// for all $\sigma \in S$ by 
// \begin{equation*}
// \footnotesize
// \operatorname{iter}\llbracket \verb|FOR(|x\verb|=|E\verb|,| E' \verb|,+)| \rrbracket (\sigma)= 
// \begin{cases}
// \zeta, & \text{if } \{\mathcal{E}\segcc{E}(\sigma),\mathcal{E}\segcc{E'}(\sigma) \} \nsubseteq \intcc{-n_\mathrm{max};n_\mathrm{max} } \\
// (x,0,0,0), & \text{else if } \mathcal{E}\segcc{E}(\sigma) > \mathcal{E}\segcc{E'} (\sigma) \\
// ( x, \mathcal{E}\segcc{E}(\sigma),  \mathcal{E}\segcc{E'} (\sigma), 1), & \text{otherwise}
// \end{cases}
// \end{equation*}
// and 
// \begin{equation*}
//  \footnotesize
// \operatorname{iter}\llbracket \verb|FOR(|x\verb|=|E\verb|,| E' \verb|,-)| \rrbracket (\sigma) = 
// \begin{cases}
// \zeta, & \text{if } \{\mathcal{E}\segcc{E}(\sigma),\mathcal{E}\segcc{E'}(\sigma) \} \nsubseteq \intcc{-n_\mathrm{max};n_\mathrm{max} } \\
// (x,0,0,0), & \text{else if } \mathcal{E}\segcc{E} (\sigma) < \mathcal{E}\segcc{E'} (\sigma) \\
// ( x, \mathcal{E}\segcc{E}(\sigma),  \mathcal{E}\segcc{E'}(\sigma), -1), & \text{otherwise}
// \end{cases}.
// \end{equation*}
// \end{aux}
// \begin{semantics}
// Define $$\mathcal{I} \colon [\verb|iteration_head|] \to \{ S \to S \}$$ for all $\sigma \in S$ by
// \begin{enumerate}
// \item With $w = \verb|FOR(|x\verb|=|E\verb|,| E' \verb|,+)|$ define
// \begin{equation*}
// \mathcal{I}\llbracket w \rrbracket (\sigma) = 
// \begin{cases}
// \mathbf{error} , & \text{if } \operatorname{iter}\segcc{w}(\sigma) = \zeta \\ 
// \mathbf{error} , & \text{else if } \sigma(x)_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \sigma ' , & \text{otherwise}
// \end{cases}
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{itemize}
// \renewcommand\labelitemi{$\cdot$}
// \item $\sigma'(x)_{\mathrm{type}} =\mathbf{Real}$,
// \item $\sigma'(x)_\mathrm{rowdim} = \sigma'(x)_\mathrm{coldim} = 1$.
// \end{itemize}
// \item $\mathcal{I}\llbracket \verb|FOR(|x\verb|=|E\verb|,| E' \verb|,-)|\rrbracket$ = $\mathcal{I}\llbracket \verb|FOR(|x\verb|=|E\verb|,| E' \verb|,+)|\rrbracket$.
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
iteration_statement
	: iteration_head compound_statement                    { $$ = install_statement(ITER,(void *)build_iteration($1,$2));   }
	;
//\end{verbatim}
// \begin{semantics}
// Let $N_1 =  \verb|iteration_head|$, $N_2 = \verb|statement_list|$, 
// $a_i \in [N_i]$, and let $\mathcal{I}_i$ be the
// interpretation function for $N_i$, $i \in \{1,2\}$. Define
// $$\mathcal{I} \colon [\verb|iteration_statement|] \to \{S \to S \}$$ for all $\sigma \in S$ by
// \begin{equation*}
// \mathcal{I}\llbracket a_1 \ \verb|{| a_2 \verb|}| \rrbracket (\sigma) = \begin{cases} 
// \mathbf{error}, & \text{if } \mathcal{I}_1\segcc{a_1}(\sigma)= \mathbf{error} \\
// \sigma', & \text{else if } (\operatorname{iter}\segcc{a_1}(\sigma))_4 = 1 \\
// \sigma '' , & \text{else if } (\operatorname{iter}\segcc{a_1}(\sigma))_4 = -1 \\
// \sigma ,  & \text{otherwise}
// \end{cases}
// \end{equation*}
// where 
// \begin{enumerate} 
// \item $\sigma '$ coincides with 
// \begin{equation}
// \label{e:iter:1}
// \mathcal{I}_2\llbracket x \verb| = |k \verb|;{|a_2\verb|} |x\verb| = | k \verb|+1;| \verb|{|a_2\verb|}| \ldots x\verb| = | l \verb|;| \verb|{|a_2\verb|}| \rrbracket(\mathcal{I}_1\segcc{a_1}(\sigma)) 
// \end{equation}
// except that $\sigma'(x) = \sigma_0(x)$, where $(x,k,l,1) = \operatorname{iter}\segcc{a_1}(\sigma)$. 
// If \ref{e:iter:1} equals $\mathbf{error}$ then $\sigma'= \mathbf{error}$;
// \item
// $\sigma ''$ coincides with
// \begin{equation}
// \label{e:iter:2}
// \mathcal{I}_2\llbracket x \verb| = |k \verb|;{|a_2\verb|} |x\verb| = | k \verb|-1;| \verb|{|a_2\verb|}| \ldots x\verb| = | l \verb|;| \verb|{|a_2\verb|}|  \rrbracket(\mathcal{I}_1\segcc{a_1}(\sigma))
// \end{equation}
// except that $\sigma''(x) = \sigma_0(x)$, where $(x,k,l,-1) = \operatorname{iter}\segcc{a_1}(\sigma)$.
// If \ref{e:iter:2} equals $\mathbf{error}$ then $\sigma''= \mathbf{error}$;
// \end{enumerate}
// \begin{remarks}
// By the definition of $\sigma'$ and $\sigma ''$, the iteration control variable 
// $x$ possesses a meaning only within the iteration construct.
// \end{remarks}
// \end{semantics}
// \end{production}
// \newpage
// \subsubsection{Real and interval expressions}
// \begin{production}
//\begin{verbatim}
expression_or_constant_interval_expression
	: expression			              { $$ = $1 ; }
	| constant_interval_expression         { $$ = $1 ; }
	;
//\end{verbatim}
// \begin{aux}
// Let $N_1  \defas \verb|expression_or_constant_interval_expression|$.
// We define auxiliary functions $\operatorname{IsNumber}$ and $\operatorname{IsBoundedInterval}$ on $[N_1]$, 
// which determine whether $E \in [N_1]$ can be interpreted as an element of $\mathbb{R} \cup F_{\overline{\mathbb{R}}}$ or $I_\mathbb{R} \setminus \mathbb{R}$, respectively. Let $\mathcal{I}_1$ and $\mathcal{I}_2$ be the interpretation functions
// for $\verb|expression|$ and $\verb|constant_interval_expression|$.
// Define 
// \begin{equation*}
// \label{e:IsNumber}
// \operatorname{IsNumber} \colon [\verb|expression_or_constant_interval_expression|] \to \{S \to \{0,1\}\}
// \end{equation*}
// for all $\sigma \in S$ by 
// \begin{equation*}
// \operatorname{IsNumber}\segcc{E}(\sigma) = \begin{cases} 0, & \text{if } \sigma = \mathbf{error} \\
// 0, & \text{else if } E \in [\verb|constant_interval_expression|] \\
// 0, & \text{else if } E \in [\verb|postfix_expression|] \\
// &\text{and } (\operatorname{IsEntry}\segcc{E}(\sigma) = 0 \text{ or } \sigma(\operatorname{name}\segcc{E})_\mathrm{type} \neq \mathbf{Real}) \\
// 0, & \text{else if } E \in [\verb|postfix_expression|] \\
// &\text{and } (\operatorname{ReadEntry}\segcc{E}(\sigma))_1 = \zeta \\
// 0, & \text{else if } E \in [\verb|expression|] \text{ and }\mathcal{I}_1\segcc{E}(\sigma) = \zeta \\
// 1, & \text{otherwise} \end{cases},
// \end{equation*}
// and 
// \begin{equation*}
// \label{e:IsBoundedInterval}
// \operatorname{IsBoundedInterval} \colon [\verb|expression_or_constant_interval_expression|] \to \{S \to \{0,1\}\}
// \end{equation*}
// for all $\sigma \in S$ by 
// \begin{equation*}
// \operatorname{IsBoundedInterval}\segcc{E}(\sigma) = \begin{cases} 0, & \text{if } \sigma = \mathbf{error}, \\
// 0, & \text{else if } E \in [\verb|expression|] \setminus [\verb|postfix_expression| ] \\
// 0, & \text{else if } E \in [\verb|postfix_expression|] \text{ and}  \\
// &(\operatorname{IsEntry}\segcc{E}(\sigma) = 0 \text{ or } \sigma(\operatorname{name}\segcc{E})_\mathrm{type} \neq \mathbf{Interval}) \\
// 0, & \text{else if } E \in [\verb|postfix_expression|] \\
// &\text{and } (\operatorname{ReadEntry}\segcc{E}(\sigma))_2 = \mathbb{R} \\
// 0, & \text{else if } E \in [\verb|constant_interval_expression|] \\
// &\text{and } \mathcal{I}_2\segcc{E}(\sigma) = \zeta \\
// 1, & \text{otherwise} \end{cases}.
// \end{equation*}
// \end{aux}
// \end{production}
// \subsubsection{Constants}
// \label{ss:numbers}
//
// \begin{production}
//\begin{verbatim}
constant_number
	: NNINTEGER             { $$ = install_Number(NULL,NUM_INT,yytext);      }
	| DECIMAL               { $$ = install_Number(NULL,NUM_DEC,yytext);      }
	| _PI                   { $$ = install_Number(NULL,NUM_CONST,yytext);    } /* mathematical constants, e.g. Pi = 3.14159... */ 
	;  
// \end{verbatim}
// \begin{semantics}
// Let $a_i \in [N_i]$, $i \in \{1,2\}$, where
// ~\vspace{.3cm}\\
// \begin{tabular}{ll}
// &$N_1 = \verb|NNINTEGER|$, \\
// &$N_2 = \verb|DECIMAL|$. 
// \end{tabular}
// ~\vspace{.3cm}\\
// Let $\mathcal{I}_i$ be the interpretation function for $N_i$, $i \in \{1,2\}$.
// Define
// \begin{equation*}
// \mathcal{I} \colon [\verb|constant_number|] \to \{ S \to \overline{\mathbb{R}} \}
// \end{equation*}
// for all $\sigma \in S$ by 
// \begin{equation*}
// \mathcal{I}\segcc{a_i}(\sigma) = \begin{cases} 
// \zeta, & \text{if } \sigma = \mathbf{error} \\ \mathcal{I}_i\segcc{a_i}, & \text{otherwise} \end{cases}, 
// \end{equation*}
// and 
//\begin{equation*}
// \mathcal{I}\llbracket \verb|_PI| \rrbracket(\sigma) =\begin{cases}
// \zeta, & \text{if } \sigma = \mathbf{error} \\ \pi, & \text{otherwise} \end{cases},
//\end{equation*}
// where $\pi$ is the ratio of the circumference of a circle to its diameter.
// \end{semantics}
// \end{production}
// \subsubsection{Arithmetic expressions}
// 
// \label{ss:expression}
// The Backus-Naur forms in the present section are required to give a meaning to, e.g., the word \verb|2*Pi| in line 2 in \ref{fig:1} and lines 8 and 9. 
// A significant difference between line 2 and lines 8, 9 is that the latter lines belong explicitly to a representation of a function
// while the former does not. This difference is reflected in the two cases 
// \begin{enumerate}
// \item \label{l:without} $\sigma \notin S_\mathrm{dom}$
// \item \label{l:with} $\sigma \in S_\mathrm{dom}$
// \end{enumerate}
// where
// \begin{equation*}
// S_\mathrm{dom} = \{ \sigma \in S \mid \sigma \text{ satisfies } \ref{d:dom} \} \cup \{ \mathbf{error} \} \ \text{ and}
// \end{equation*}
// \begin{hypothesisA}
// \label{d:dom}
// $0 < n \defas |\{ x \in A \mid \sigma(x)_\mathrm{role} = \mathbf{Input} \}| < \infty$ 
// and there exist $x_1,\ldots,x_{n} \in A$ such that
// for every $i \in \intcc{1;n}$ the components 
// $\mathrm{role}$, $\mathrm{rowdim}$ and $\mathrm{idx}$ of $\sigma(x_i)$ equal
// $\mathbf{Input}$, $k_i$ and $q_i>0$, respectively, and $q_i < q_{i+1}$ for every $i \in \intco{1;n}$.
// \end{hypothesisA}
// We recall that a program defines a finite sequence in $S$, so considering again \ref{fig:1},
// we remark that the programming language is finally defined such that 
// the state after line 2 is not contained in $S_\mathrm{dom}$ while the states after lines 8 and 9 are contained in $S_\mathrm{dom}$. \par
// We note that the integers $q_i$ in \ref{d:dom} fix an order on 
// the identifiers $x_1,\ldots,x_n$, 
// and we intentionally do not use $i$ in place of $q_i$. (See Section \ref{ss:auxode}.)
// \begin{production}
//\begin{verbatim}
number_or_variable
	: constant_number       { $$ = $1 ; } 
	| postfix_expression    { $$ = $1 ; }
	;  
// \end{verbatim}
// \begin{semantics}
// Let $a_i \in [N_i]$, $i \in \{1,2\}$, where
// ~\vspace{.3cm}\\
// \begin{tabular}{ll}
// &$N_1 = \verb|constant_number|$, \\
// &$N_2 = \verb|postfix_expression|$.
// \end{tabular}
// ~\vspace{.3cm}\\
// Let $\mathcal{E}$ be the interpretation function for $N_1$.
// Define
// \begin{equation*}
// \mathcal{I} \colon [\verb|number_or_variable|] \to \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}
// \end{equation*}
// for all $\sigma \in S$ by $\mathcal{I}\segcc{a_1}(\sigma) = \mathcal{E}\segcc{a_1}(\sigma)$ and
//\begin{equation*}
// \small
// \mathcal{I}\segcc{a_2}(\sigma) = \begin{cases}
// \zeta, & \text{if } \sigma = \mathbf{error} \\
// \zeta, & \text{else if }\operatorname{IsEntry}\segcc{a_2}(\sigma) = 0 \\ 
// \zeta, & \text{else if }\sigma(\operatorname{name}\segcc{a_2})_\mathrm{role} \notin \{ \mathbf{Ordinary},\mathbf{Input},\mathbf{Output}\}\\
// \zeta, & \text{else if } \sigma \notin S_\mathrm{dom} \text{ and } \sigma(\operatorname{name}\segcc{a_2})_\mathrm{role} = \mathbf{Input} \\
// (x_1,\ldots,x_n) \mapsto (x_{j})_{r}, & \text{else if } \sigma \in S_\mathrm{dom} \text{ and } \sigma(\operatorname{name}\segcc{a_2})_\mathrm{role} = \mathbf{Input} \\
// (\operatorname{ReadEntry}\segcc{a_2}(\sigma))_1, & \text{otherwise} \end{cases},
//\end{equation*}
// where $r = 1+\operatorname{row}_0\segcc{a_2}(\sigma)$, $n$, $q_j$ as in \ref{d:dom} for $\sigma$ and 
// $j$ such that $\sigma(\operatorname{name}\segcc{a_2})_\mathrm{idx} = q_j$.
// Moreover, in case 5, $\mathcal{I}\segcc{a_2}(\sigma)$ is
// a function of $n$
// variables that correspond to the identifiers $x_1,\ldots,x_n$ in \ref{d:dom}, where 
// the domain is defined through the second components of the entries 
// of $\sigma(x_i)_{\mathrm{data}}$, $i \in \intcc{1;n}$. 
// See also Section \ref{ss:auxfunctions}. \\
// \end{semantics}
// \end{production}
// \par
// Subsequently, we use extended versions of the elementary operations, i.e., 
// we let $g(x,\zeta) = g(\zeta,x) = \zeta$ and 
// $x/0 = \zeta$ for all $x \in \overline{\mathbb{R}}$, where
// $g \in \{+,-,\cdot,\div\}$. \\
// \begin{production}
//\begin{verbatim}
expression
	: multiplicative_expression			   { $$ = $1                              ; }
	| expression '+' multiplicative_expression { $$ = install_expression(PLUS,$1,$3)  ; }
	| expression '-' multiplicative_expression { $$ = install_expression(MINUS,$1,$3) ; }
	;
//\end{verbatim}
// \begin{semantics}
// Let $N_1 = \verb|multiplicative_expression|$, $a \in [N_1]$, let $\mathcal{E}$ be the interpretation function
// for $N_1$, and $b \in [\verb|expression|]$. Define
// $$ \mathcal{I} \colon [\verb|expression|] \to \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}$$ for all $\sigma \in S$ by
// \begin{enumerate}
// \item \label{l:expression:1} $\mathcal{I}\segcc{a} = \mathcal{E}\segcc{a}$, 
// \item \label{l:expression:2} $\mathcal{I}\llbracket b \verb|+| a \rrbracket (\sigma) = 
// \mathcal{I}\segcc{b}(\sigma) + \mathcal{E}\segcc{a}(\sigma)$, 
// \item \label{l:expression:3} $\mathcal{I}\llbracket b \verb|-| a \rrbracket (\sigma) = 
// \mathcal{I}\segcc{b}(\sigma) - \mathcal{E}\segcc{a}(\sigma)$.
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
multiplicative_expression
	: signed_expression                               { $$ = $1                              ; }
	| multiplicative_expression '*' signed_expression { $$ = install_expression(TIMES,$1,$3) ; }
	| multiplicative_expression '/' signed_expression { $$ = install_expression(DIV,$1,$3)   ; }
	;
//\end{verbatim}
// \begin{semantics}
// Let $N_1 = \verb|signed_expression|$, $a \in [N_1]$,
// $b \in [ \verb|multiplicative_expression|]$, and let $\mathcal{E}$ be
// the interpretation function for $N_1$. Define
// $$\mathcal{I} \colon [ \verb|multiplicative_expression| ] \to \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}$$ 
// for all $\sigma \in S$ by
// \begin{enumerate}
// \item $\mathcal{I}\segcc{a} = \mathcal{E}\segcc{a}$, 
// \item $\mathcal{I}\llbracket b \verb|*| a \rrbracket (\sigma) = \mathcal{I}\segcc{b}(\sigma) \cdot \mathcal{E}\segcc{a}(\sigma) $, 
// \item $\mathcal{I}\llbracket b \verb|/| a \rrbracket (\sigma) = \mathcal{I}\segcc{b}(\sigma) / \mathcal{E}\segcc{a}(\sigma) $.
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
signed_expression
	: exponential_expression                         { $$ = $1 ;                              }
	| '-' exponential_expression                     { $$ = install_expression(MINUS,install_Number(NULL,NUM_INT,"0"),$2) ; }
	| '+' exponential_expression                     { $$ = $2 ;                              }
	;
//\end{verbatim}
// \begin{semantics}
// Let $N_1 = \verb|exponential_expression|$, $a \in [N_1]$ and $\mathcal{E}$ the interpretation function for $N_1$. 
// Define $$\mathcal{I} \colon [\verb|signed_expression|] \to  \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}$$
// for all $\sigma \in S$ by 
// \begin{enumerate}
// \item $\mathcal{I}\segcc{a} = \mathcal{E}\segcc{a}$, 
// \item $\mathcal{I}\llbracket \verb|-| a \rrbracket (\sigma) = 0 - \mathcal{E}\segcc{a}(\sigma) $, 
// \item $\mathcal{I}\llbracket \verb|+| a \rrbracket (\sigma) = \mathcal{E}\segcc{a}(\sigma) $.
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
exponential_expression
	: primary_expression
	| primary_expression '^' signed_expression  { $$ = install_expression(EXP,$1,$3)   ; } /* exponentiation */
	;
//\end{verbatim}
// \begin{semantics}
// Let $N_1 =  \verb|primary_expression|$, $a \in [N_1]$, $N_2 = \verb|signed_expression|$, $\mathcal{I}_i$ the interpretation function for $N_i$, $i \in \{1,2\}$,
// and $b \in [ N_2 ]$.
// Define $$\mathcal{I} \colon [\verb|exponential_expression| ] \to  \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}$$ 
// for all $\sigma \in S$ by 
// \begin{enumerate}
// \item $\mathcal{I}\segcc{a} = \mathcal{I}_1\segcc{a}$, 
// \item 
// \begin{equation*}
// \mathcal{I}\llbracket a \verb|^|b \rrbracket (\sigma) = \begin{cases}
// (\mathcal{I}_1\segcc{a}(\sigma))^{\mathcal{I}_2\segcc{b}(\sigma)}, & \text{if } \mathcal{I}_1\segcc{a}(\sigma),\mathcal{I}_2\segcc{b}(\sigma) \in \mathbb{Z}, \\
// \zeta, & \text{else if } \mathcal{I}_2\segcc{b}(\sigma) = \zeta, \\
// \zeta, & \text{else if } \mathcal{I}_1\segcc{a}(\sigma) = \zeta, \\
// \zeta, & \text{else if } \mathcal{I}_1\segcc{a}(\sigma) \leq 0, \\
// \exp\big (\mathcal{I}_2\segcc{b}(\sigma) \cdot \ln(\mathcal{I}_1\segcc{a}(\sigma)) \big ) & \text{otherwise}.
// \end{cases}
// \end{equation*}
// \end{enumerate}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
primary_expression
	: '(' expression ')'                         { $$ = $2 ;                              }
	| call_function  						{ $$ = $1 ;                              }
	| number_or_variable								{ $$ = $1 ;                              }
	;
//\end{verbatim}
// \begin{semantics}
// Let $\mathcal{E}$ be the interpretation function for $\verb|expression|$ and $E \in [\verb|expression|]$. Define 
// $$\mathcal{I} \colon [\verb|primary_expression|] \to \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}$$
// by $\mathcal{I}\llbracket \verb|(| E  \verb|)| \rrbracket (\sigma) = \mathcal{E}\segcc{E}(\sigma)$ and in the 
// other cases as the interpretation function of the alternative.
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
call_function
	: _ATAN '(' expression ')' { $$ = install_expression(FCT_ATAN,NULL,$3) ; }
	| _COS  '(' expression ')' { $$ = install_expression(FCT_COS,NULL,$3)  ; }
	| _COSH '(' expression ')' { $$ = install_expression(FCT_COSH,NULL,$3) ; }
	| _EXP  '(' expression ')' { $$ = install_expression(FCT_EXP,NULL,$3)  ; }
	| _LN   '(' expression ')' { $$ = install_expression(FCT_LOG,NULL,$3)  ; }
	| _SIN  '(' expression ')' { $$ = install_expression(FCT_SIN,NULL,$3)  ; }
	| _SINH '(' expression ')' { $$ = install_expression(FCT_SINH,NULL,$3) ; }
	| _SQRT '(' expression ')' { $$ = install_expression(FCT_SQRT,NULL,$3) ; }
	| _TAN  '(' expression ')' { $$ = install_expression(FCT_TAN,NULL,$3)  ; }
	; 
//\end{verbatim}
// \begin{semantics}
// Let $\mathcal{E}$ be the interpretation function for $\verb|expression|$, $E \in [\verb|expression|]$.
// Define 
// $$\mathcal{I} \colon [\verb|call_function|] \to \{ S \to \overline{\mathbb{R}} \cup F_{\overline{\mathbb{R}}} \}$$
// by
// \begin{equation*}
// \mathcal{I}\llbracket a \verb|(| E \verb|)| \rrbracket (\sigma ) \begin{cases} 
// \zeta , & \text{if } \mathcal{E}\segcc{E}(\sigma) \notin \dom f \\
// f(\mathcal{E}\segcc{E}(\sigma)) , & \text{otherwise}
// \end{cases}, 
// \end{equation*}
// with using 
// \begin{enumerate}
// \item $a= \verb|_ATAN|$ and $f = \operatorname{arctan}$ (arctangent).
// \item $a= \verb|_COS|$ and $f= \cos$ (cosine). 
// \item $a= \verb|_COSH|$ and $f= \cosh$ (hyperbolic cosine).  
// \item $a= \verb|_EXP|$ and $f= \exp$ (exponential).  
// \item $a = \verb|_LN|$ and $f= \ln$ (natural logarithm).  
// \item $a= \verb|_SIN|$ and $f = \sin$ (sine).
// \item $a= \verb|_SINH|$ and $f= \sinh$ (hyperbolic sine).
// \item $a= \verb|_SQRT|$ and $f=(x \mapsto \sqrt{x})$ (square root).    
// \item $a= \verb|_TAN|$ and $f= \tan$ (tangent).
// \end{enumerate}
// \end{semantics}
// \end{production}
// \subsubsection{Constant interval expressions}
// The Backus-Naur forms in this section are used to represent elements in $I_\mathbb{R}$ and the real space $\mathbb{R}^k$, $k \in \mathbb{N}$.
// \begin{production}
//\begin{verbatim}
constant_interval_expression
	: continuous_interval_expression
	| discrete_interval_expression
	;
//\end{verbatim}
// \begin{semantics}
// The interpretation function for $\verb|constant_interval_expression|$ is defined through the 
// interpretation function of the respective alternative.
// \end{semantics}
// \end{production}
// \begin{production}
// \begin{verbatim}
continuous_interval_expression
	: '[' expression ',' expression ']' { $$ = install_expression(CONT_INTV,$2,$4) ; }
	;
// \end{verbatim}
// \begin{semantics}
// Let $\mathcal{E}$ be the interpretation function for $\verb|expression|$, $E,E' \in [\verb|expression|]$. 
// Define 
// $$ \mathcal{I} \colon [\verb|continuous_interval_expression|] \to \{ S \to I_{\mathbb{R}} \cup \{ \zeta\} \}$$ for all $\sigma \in S$ by
// \begin{equation*}
// \mathcal{I}\segcc{a}(\sigma) = \begin{cases} 
// \zeta, & \text{if } \{\mathcal{E}\segcc{E}(\sigma),\mathcal{E}\segcc{E'}(\sigma)\} \nsubseteq \mathbb{R} \\
// \intcc{\mathcal{E}\segcc{E'}(\sigma),\mathcal{E}\segcc{E}(\sigma) }, & \text{else if } \mathcal{E}\segcc{E}(\sigma) > \mathcal{E}\segcc{E'}(\sigma) \\
// \intcc{\mathcal{E}\segcc{E}(\sigma),\mathcal{E}\segcc{E'}(\sigma) } , & \text{otherwise} \end{cases}
// \end{equation*}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
discrete_interval_expression 
	: '[' expression ';' expression ']' { $$ = install_expression(DISC_INTV,$2,$4) ; }
	;
//\end{verbatim}
// \begin{semantics}
// Let $\mathcal{E}$ be the interpretation function for $\verb|expression|$, $E,E' \in [\verb|expression|]$. 
// Define 
// $$ \mathcal{I} \colon [\verb|discrete_interval_expression|] \to \{ S \to I_{\mathbb{R}} \cup \{ \zeta\} \}$$ for all $\sigma \in S$ by
// \begin{equation*}
// \mathcal{I}\segcc{a}(\sigma) = \begin{cases} 
// \zeta, & \text{if } \{\mathcal{E}\segcc{E}(\sigma),\mathcal{E}\segcc{E'}(\sigma) \} \nsubseteq \mathbb{R} \\
// \intcc{\mathcal{E}\segcc{E'}(\sigma);\mathcal{E}\segcc{E}(\sigma) }, & \text{else if } \mathcal{E}\segcc{E}(\sigma) > \mathcal{E}\segcc{E'}(\sigma) \\
// \intcc{\mathcal{E}\segcc{E}(\sigma);\mathcal{E}\segcc{E'}(\sigma) } , & \text{otherwise} \end{cases}
// \end{equation*}
// \end{semantics}
// \end{production}
// \begin{production}
//\begin{verbatim}
id_or_intv_expr_or_R_n
	: constant_interval_expression			{ $$ = $1 ; }
	| identifier					{ $$ = $1 ; }
	| REAL						{ $$ = install_expression(REAL_SPACE,NULL,NULL); }
	| REAL '[' expression ']'		{ $$ = install_expression(REAL_SPACE,NULL,$3)  ; }
	;
//\end{verbatim}
// \begin{semantics}
// For $i \in \mathbb{N}$ the $i$-fold Cartesian product 
// $I_\mathbb{R} \times \ldots \times I_\mathbb{R}$ is denoted by $I_\mathbb{R}^i$ below.
// Let \vspace{.3cm} \\
// \begin{tabular}{ll}
// &$a_1 \in [\verb|constant_interval_expression|]$, \\
// &$a_2 \in [\verb|identifier|]$, \\
// &$E \in [\verb|expression|]$.
// \end{tabular} ~\vspace{.3cm}\\
// Let $\mathcal{I}_1$ and $\mathcal{E}$ be the interpretation functions for $ \verb|constant_interval_expression|$ and $\verb|expression|$, respectively.
// Define $$\mathcal{I} \colon [ \verb|id_or_intv_expr_or_R_n| ] \to \Big \{ S \to \big (\bigcup_{i\in \mathbb{N}}\nolimits I_\mathbb{R}^i \big ) \cup \{\zeta\} \Big \}$$
// for all $\sigma \in S$ by 
// \begin{enumerate}
// \item $\mathcal{I}\segcc{a_1}(\sigma) = \mathcal{I}_1\segcc{a_1}(\sigma)$,
// \item
// \footnotesize
// \begin{equation*}
// \mathcal{I}\segcc{a_2}(\sigma) = \begin{cases} 
// \zeta, & \text{if } \sigma = \mathbf{error} \\
// \zeta, & \text{else if } \sigma(a_2)_\mathrm{type} \neq \mathbf{Interval} \\
// \zeta, & \text{else if } \sigma(a_2)_\mathrm{coldim} \neq 1 \\
// \zeta, & \text{else if}\footnote{The second components in $\mathbf{D}$ related to the identifier $a_2$ shall not be as in the initial state $\sigma_0$. Thus, an identifier cannot represent an unbounded interval. } \ \exists_{i \in \intcc{1;n} } \big ((\sigma(a_2)_{\mathrm{data}})_{i,1}\big )_2 = \mathbb{R} \\
// \big ((\sigma(a_2)_{\mathrm{data}})_{1,1} \big )_2 \times \ldots \times \big ((\sigma(a_2)_{\mathrm{data}})_{n,1} \big )_2, &  \text{otherwise}
// \end{cases},
// \end{equation*}
// \normalsize
// where $n = \sigma(a_2)_\mathrm{rowdim}$,
// \item
// $$\mathcal{I}\llbracket \verb|REAL| \rrbracket (\sigma) = \begin{cases} \zeta, & \text{if } \sigma = \mathbf{error} \\ \mathbb{R}, & \text{otherwise} \end{cases},$$
// \item
// $$\mathcal{I}\llbracket \verb|REAL[| E \verb|]| \rrbracket (\sigma) = \begin{cases} \zeta, & \text{if } \sigma = \mathbf{error} \\
// \zeta, & \text{else if } \mathcal{E}\segcc{E}(\sigma) \notin \intcc{1;n_\mathrm{max} } \\ \mathbb{R}^{\mathcal{E}\segcc{E}(\sigma)}, & \text{otherwise} \end{cases}.$$
// \end{enumerate}
// \end{semantics}
// \begin{aux}
// The subsequent function is used in combination with the nonterminal $\verb|function_head|$ in Section \ref{ss:Functions}. 
// Let $\mathcal{E}$, $a_1$, $a_2$ be as above.
// Define $$\operatorname{dimension} \colon [ \verb|id_or_intv_expr_or_R_n| ] \to \{ S \to \mathbb{Z}_+\}$$ for all $\sigma \in S$ by 
// \begin{enumerate}
// \item $$\operatorname{dimension}\segcc{a_1}(\sigma) = \begin{cases} 0 , & \text{if } \sigma = \mathbf{error}  \\ 0 , & \text{else if }  \mathcal{I}\segcc{a_1}(\sigma)= \zeta \\ 
// 1 , & \text{otherwise} \end{cases},$$
// \item $$\operatorname{dimension}\segcc{a_2}(\sigma) = \begin{cases} 0 , & \text{if } \sigma = \mathbf{error}  \\ 0 , & \text{else if } \mathcal{I}\segcc{a_2}(\sigma)= \zeta \\ 
// \sigma(a_2)_\mathrm{rowdim} , & \text{otherwise} \end{cases},$$
// \item $$\operatorname{dimension}\llbracket \verb|REAL| \rrbracket (\sigma) = \begin{cases} 0  & \text{if } \sigma = \mathbf{error}  \\ 
// 1 , & \text{otherwise} \end{cases},$$
// \item $$\operatorname{dimension}\llbracket \verb|REAL[| E \verb|]| \rrbracket (\sigma) = \begin{cases} 0 , & \text{if } \sigma = \mathbf{error}  \\ 0 , & \text{else if } \mathcal{I}\llbracket \verb|REAL[| E \verb|]| \rrbracket(\sigma) = \zeta \\ 
// \mathcal{E}\segcc{E}(\sigma) , & \text{otherwise} \end{cases}.$$
// \end{enumerate}
// \end{aux}
// \end{production}
// \subsubsection{Lists and tuples}
// \begin{production}
//\begin{verbatim}
identifier_list         
	: identifier                        { $$ = $1; }
	| identifier_list ',' identifier    { $$ = (Identifier)append(AST_EXPR,(void *)$1,(void *)$3); }
	;
//\end{verbatim}
// %\begin{aux}
// %Let $x\in [\verb|identifier|]$, $y \in [\verb|identifier_list|]$.
// %Define $$\mathrm{set} \colon [\verb|identifier_list| ] \rightrightarrows [ \verb|identifier|]$$ by 
// %\begin{equation*}
// %\mathrm{set}\segcc{x} = \{ x \},
// %\end{equation*}
// %\begin{equation*}
// %\mathrm{set}\llbracket y \verb|,| x \rrbracket = \operatorname{set}\segcc{y} \cup \operatorname{set}\segcc{x}.
// %\end{equation*}
// %Define $$\operatorname{dimension} \colon [ \verb|identifier_list|] \to \{ S \to \mathbb{N} \cup \{ \zeta \} \}$$ by
// %\begin{equation*}
// %\operatorname{dimension}\segcc{x} (\sigma) = \begin{cases} \zeta, & \text{if } \sigma = \mathbf{error} \\
// %\zeta, &  \text{else if } \sigma(x)_\mathrm{type} = \mathbf{undeclared} \\
// %\zeta, & \text{else if } \sigma(x)_\mathrm{coldim} > 1 \\
// %\sigma(x)_\mathrm{rowdim}, & \text{otherwise} \end{cases}
// %\end{equation*}
// %\begin{equation*}
// %\operatorname{dimension}\llbracket y \verb|,| x \rrbracket (\sigma)= \begin{cases} \zeta, & \text{if } \operatorname{dimension}\segcc{y}(\sigma) = \zeta \\
// %\zeta, & \text{else if } \operatorname{dimension}\segcc{x}(\sigma) = \zeta \\
// %\operatorname{dimension}\segcc{y}(\sigma) + \operatorname{dimension}\segcc{x}(\sigma), & \text{otherwise} \end{cases}
// %\end{equation*}
// %\end{aux}
// \end{production}
// \begin{production}
//\begin{verbatim}
identifier_tuple
	: '(' identifier_list ')'           { $$ = $2 ;              }
	;
//\end{verbatim}
// \end{production}
//\begin{production}
//\begin{verbatim}
postfix_expression_list  
	: postfix_expression                                   { $$ = $1 ;  }
	| postfix_expression_list ',' postfix_expression       { $$ = (Identifier)append(AST_EXPR,(void *)$1,(void *)$3);             }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
postfix_expression_tuple  
	: '(' postfix_expression_list ')' { $$ = $2 ;  }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
id_or_intv_expr_or_R_n_list
	: id_or_intv_expr_or_R_n                              { $$ = $1 ; }
	| id_or_intv_expr_or_R_n_list ',' id_or_intv_expr_or_R_n { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3) ;    }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
id_or_intv_expr_or_R_n_tuple
	: '(' id_or_intv_expr_or_R_n_list ')' { $$ = install_expression(LIST,$2,NULL) ; }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
nested_id_or_intv_expr_or_R_n_list
	: id_or_intv_expr_or_R_n
	| id_or_intv_expr_or_R_n_tuple
	| nested_id_or_intv_expr_or_R_n_list ',' id_or_intv_expr_or_R_n     { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3) ;    }
	| nested_id_or_intv_expr_or_R_n_list ',' id_or_intv_expr_or_R_n_tuple  { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3) ;     }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
nested_id_or_intv_expr_or_R_n_tuple
	: '(' nested_id_or_intv_expr_or_R_n_list ')'  { $$ = $2 ; }
	;
//\end{verbatim}
// \end{production}
// \subsubsection{Functions}
// \label{ss:Functions}
// The Backus-Naur forms in this section allow to represent functions in the set $\bar F$ that are defined through an explicit formula, where 
// $\bar F$ is the set in \ref{e:F} but with 
// $Y = \overline{\mathbb{R}}^{k_1 \times l_1} \times \ldots \times \overline{\mathbb{R}}^{k_m \times l_m}$ 
// in place of $Y = \mathbb{R}^{k_1 \times l_1} \times \ldots \times \mathbb{R}^{k_m \times l_m}$. The central nonterminal is 
// $\verb|function_definition|$ for which we need the following subset $S_\mathrm{fct}$ of states. Intuitively speaking, $\sigma \in S_\mathrm{fct}$ contains a representation of a function in $\bar F$. (Details are given in Section \ref{ss:auxfunctions}.) Define
// \begin{equation*}
// S_\mathrm{fct} = \{ \sigma \in S \mid \sigma \text{ satisfies } \ref{d:dom} \text{ and } \ref{d:fct} \} \cup \{ \mathbf{error}\}, \text{ where}
// \end{equation*}
// \begin{hypothesisA}
// \label{d:fct}
// $0< m \defas |\{x \in A \mid  \sigma(x)_\mathrm{role} = \mathbf{Output} \}| < \infty$ and there exist
// $y_1,\ldots,y_m \in A$ such that for every $i \in \intcc{1;m}$ the components 
// $\mathrm{role}$ and $\mathrm{idx}$ of $\sigma(y_i)$ equal
// $\mathbf{Output}$ and $i$, respectively.
// \end{hypothesisA}
// \begin{production}
//\begin{verbatim}
function_definition     
	: function_declaration function_head compound_statement { $$ = finish_function(current_fct,$3)    ; }
	;
//\end{verbatim}
// \begin{semantics}
// Let $a_i \in N_i$, $i \in \{1,2,3\}$, where \vspace{.3cm}\\
// \begin{tabular}{ll}
// &$N_1 = \verb|function_declaration|$, \\
// &$N_2 = \verb|function_head|$, \\
// &$N_3 = \verb|compound_statement|$, \\
// \end{tabular}
// ~\vspace{.3cm}\\
// and let $\mathcal{I}_i$ be the interpretation function for $N_i$, $i \in \{1,2,3\}$. Define 
// $$\mathcal{I} \colon [\verb|function_definition|] \to \{ S \to S \}$$ for all $\sigma \in S$ by
// \begin{equation*}
// \mathcal{I}\segcc{a_1 \ a_2 \ a_3}(\sigma) = \begin{cases} \mathbf{error}, & \text{if } \sigma = \mathbf{error} \\ 
// \mathbf{error}, & \text{else if } \mathcal{I}_1\segcc{a_1}(\sigma) = \mathbf{error} \\ 
// \mathbf{error}, & \text{else if } \mathcal{I}_2\segcc{a_2} (\mathcal{I}_1\segcc{a_1}(\sigma)) = \mathbf{error} \\ 
// \mathbf{error}, & \text{else if } \mathcal{I}_2\segcc{a_2} (\mathcal{I}_1\segcc{a_1}(\sigma)) \notin S_\mathrm{fct} \\
// \mathcal{I}_3\segcc{a_3}\big ( \mathcal{I}_2\segcc{a_2} (\mathcal{I}_1\segcc{a_1}(\sigma)) \big ), & \text{otherwise} \end{cases}.
// \end{equation*}
// \end{semantics}
// \end{production}
// \begin{production}
// \begin{verbatim}
function_declaration    
	: identifier ':'                   { current_fct = install_function($1); }
	;
// \end{verbatim}
// \begin{semantics}
// Let $x \in A$. 
// Define 
// \begin{equation*}
// \mathcal{I} \colon [\verb|function_declaration|] \to \{ S \to S \}
// \end{equation*}
// by 
// \begin{equation*}
// \mathcal{I}\llbracket x \verb|:| \rrbracket (\sigma) = \begin{cases} 
// \mathbf{error}, & \text{if } \sigma = \mathbf{error} \\
// \mathbf{error}, & \text{else if } \sigma(x)_{\textrm{type}} \neq \mathbf{undeclared} \\ 
// \sigma' & \text{otherwise} \end{cases},
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{flalign*}
// &\sigma'(x)_{\mathrm{type}} = \mathbf{Function}, && \\
// &\sigma'(y) = \sigma_0(y) \text{ if } y \in A \text{ and  } \sigma(y)_{\mathrm{role}} \in \{ \mathbf{Input},\mathbf{Output} \}, \\
// &(\sigma'(y)_{\mathrm{data}})_{i,j} = (\sigma_0(y)_{\mathrm{data}})_{i,j} \text{ if } y \in A \text{ and  } ((\sigma(y)_{\mathrm{data}})_{i,j})_1 \in F_{\overline{\mathbb{R}}}.
// \end{flalign*}
// \begin{remarks}
// By the definition of $\sigma'$, identifiers representing arguments of functions or output variables possess a ``local" meaning. For example, the identifier $\verb|x|$ in \ref{fig:1} represents an argument of a function in line 6 and an output variable in line 11.  
// \end{remarks}
// \end{semantics}
// \end{production}
// \begin{production}
// \footnotesize
//\begin{verbatim}

/* to avoid conflicts in grammar 'function_head' must be written like this: */

function_head
	: identifier        IN id_or_intv_expr_or_R_n               TO postfix_expression_tuple    { build_function(IDENT,current_fct,$1,(void*)$3,$5);  } /* 2 */
	| identifier        IN id_or_intv_expr_or_R_n               TO postfix_expression          { build_function(IDENT,current_fct,$1,(void*)$3,$5);  } /* 1 */
	| identifier        IN id_or_intv_expr_or_R_n_tuple         TO postfix_expression_tuple    { build_function(MIXED,current_fct,$1,(void*)$3,$5);  } /* 4 */
	| identifier        IN id_or_intv_expr_or_R_n_tuple         TO postfix_expression          { build_function(MIXED,current_fct,$1,(void*)$3,$5);  } /* 3 */
	| identifier_tuple  IN nested_id_or_intv_expr_or_R_n_tuple  TO postfix_expression_tuple    { build_function(NESTED,current_fct,$1,(void*)$3,$5);  } /* 8 */
	| identifier_tuple  IN nested_id_or_intv_expr_or_R_n_tuple  TO postfix_expression          { build_function(NESTED,current_fct,$1,(void*)$3,$5);  } /* 7 */
	;
//\end{verbatim}
// \normalsize
// \begin{semantics}
// Let $\mathcal{I}_1$ and $\mathcal{I}_2$ be the interpretation function for $\verb|constant_interval_expression|$ and $\verb|id_or_intv_expr_or_R_n|$, respectively. Let \vspace{.3cm}\\
// \begin{tabular}{ll}
// &$x,x_1,\ldots,x_n \in [\verb|identifier|]$, \\
// &$y,y_1,\ldots,y_m \in [\verb|postfix_expression|]$, \\
// &$I,I_1,\ldots,I_p \in [\verb|id_or_intv_expr_or_R_n|]$, \\
// &$I'_{1,1},\ldots,I_{1,k_1}',\ldots,I'_{p,1},\ldots,I_{p,k_p}' \in [\verb|id_or_intv_expr_or_R_n|]$, \\
// %&$X_1,\ldots,X_p \in [\verb|id_or_intv_expr_or_R_n|] \cup [\verb|id_or_intv_expr_or_R_n_tuple|]$,
// \end{tabular}
// ~\vspace{.3cm}\\
// $n,m,p,k_1,\ldots,k_p\in \intcc{1;n_\mathrm{max}}$. Define $$ \mathcal{I} \colon [\verb|function_head|] \to \{ S \to S \}$$ for all $\sigma \in S$ by 
// \begin{enumerate}
// %********************************************************
// %********************************************************
// \item With $$w = x \verb| IN | I \verb| TO (| y_1,\ldots,y_m\verb|)|$$ let 
// \begin{equation*}
// \mathcal{I}\llbracket w \rrbracket(\sigma) = \begin{cases}
// \mathbf{error}, &\text{if } \sigma = \mathbf{error} \\
// \mathbf{error}, &\text{else if } \sigma(x)_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, &\text{else if}\footnote{If this and the next case do not occur, the identifiers $x$,$\operatorname{name}\segcc{y_1},\ldots,\operatorname{name}\segcc{y_m}$ are pairwise distinct. Similar remarks hold for \ref{l:fct:ii}--\ref{l:fct:vi}.} \ \exists_{i \in \intcc{1;m} } \ x = \operatorname{name}\segcc{y_i} \\
// \mathbf{error}, &\text{else if } \exists_{i,j \in \intcc{1;m}, i \neq j} \ \operatorname{name}\segcc{y_i} = \operatorname{name}\segcc{y_j} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;m}} \sigma(\operatorname{name}\segcc{y_i})_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, &\text{else if}\footnote{By this case, we ensure valid dimensions for the components of the image for a function represented in a program. Similar remarks hold for \ref{l:fct:ii}--\ref{l:fct:vi}.} \ \exists_{i \in \intcc{1;m}} \zeta \in \{\operatorname{row}_1\segcc{y_i}(\sigma),\operatorname{col}_1\segcc{y_i}(\sigma) \} \\  
// \mathbf{error}, &\text{else if}\footnote{By this case, we ensure a valid domain for a function represented in a program. Similar remarks hold for \ref{l:fct:ii}--\ref{l:fct:vi}.} \ \forall_{k \in \intcc{1;n_\mathrm{max}}} \mathcal{I}_2\segcc{I}(\sigma) \nsubseteq \mathbb{R}^k \\ 
// \sigma ', & \text{otherwise}
// \end{cases}
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{flalign*}
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{idx}} = i,&& \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{type}} = \mathbf{Real}, \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{role}} = \mathbf{Output}, \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{rowdim}} = \operatorname{row}_1\segcc{y_i}(\sigma), \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{coldim}} = \operatorname{col}_1\segcc{y_i}(\sigma), \\
// &\sigma'(x)_{\mathrm{idx}} = 1, \\
// &\sigma'(x)_{\mathrm{type}} = \mathbf{Real}, \\
// &\sigma'(x)_{\mathrm{role}} = \mathbf{Input}, \\
// &\sigma'(x)_{\mathrm{coldim}} = 1, \\
// &\sigma'(x)_{\mathrm{rowdim}} = \operatorname{dimension}\segcc{I}(\sigma),
// \end{flalign*}
// 
// \begin{equation*}
// \small
// \forall_{i \in \intcc{1;k} } \ \big ((\sigma'(x)_{\mathrm{data}})_{i,1}\big )_2 = 
// \begin{cases} \mathcal{I}_1\segcc{I}(\sigma), &  \text{if }  I \in [\verb|constant_interval_expression|] \\ \big ((\sigma(I)_{\mathrm{data}})_{i,1} \big )_2, & \text{else if } I \in A \\ \mathbb{R}, & \text{otherwise}
// \end{cases},
// \end{equation*}
// where $k = \sigma'(x)_{\mathrm{rowdim}}$;
// %********************************************************
// %********************************************************
// \item
// \label{l:fct:ii}
// $
// \mathcal{I}\llbracket x \verb| IN | I \verb| TO | y\rrbracket (\sigma) = \mathcal{I}\llbracket x \verb| IN | I \verb| TO (| y\verb|)|\rrbracket (\sigma);
// $
// %********************************************************
// %********************************************************
// \item With $$w = x \verb| IN (| I_1,\ldots,I_p \verb|) TO (| y_1,\ldots,y_m \verb|)|$$ let
// \begin{equation*}
// \mathcal{I}\llbracket w \rrbracket (\sigma) = \begin{cases}
// \mathbf{error}, &\text{if } \sigma = \mathbf{error} \\
// \mathbf{error}, &\text{else if } \sigma(x)_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;m} } \ x = \operatorname{name}\segcc{y_i} \\
// \mathbf{error}, &\text{else if } \exists_{i,j \in \intcc{1;m}, i \neq j} \ \operatorname{name}\segcc{y_i} = \operatorname{name}\segcc{y_j} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;m}} \sigma(\operatorname{name}\segcc{y_i})_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;m}} \zeta \in \{\operatorname{row}_1\segcc{y_i}(\sigma),\operatorname{col}_1\segcc{y_i}(\sigma) \} \\  
// \mathbf{error}, &\text{else if } p>n_\mathrm{max} \text{ or }\exists_{i \in \intcc{1;p}} \ \mathcal{I}_2\segcc{I_i}(\sigma) \nsubseteq \mathbb{R} \\ 
// \sigma ', & \text{otherwise}
// \end{cases}
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{flalign*}
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{idx}} = i,&& \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{type}} = \mathbf{Real}, \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{role}} = \mathbf{Output}, \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{rowdim}} = \operatorname{row}_1\segcc{y_i}(\sigma), \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{coldim}} = \operatorname{col}_1\segcc{y_i}(\sigma), \\
// &\sigma'(x)_{\mathrm{idx}} = 1, \\
// &\sigma'(x)_{\mathrm{type}} = \mathbf{Real}, \\
// &\sigma'(x)_{\mathrm{role}} = \mathbf{Input}, \\
// &\sigma'(x)_{\mathrm{coldim}} = 1, \\
// &\sigma'(x)_{\mathrm{rowdim}} = p,
// \end{flalign*}
// \begin{equation*}
// \small
// \forall_{i \in \intcc{1;p} } \ \big ((\sigma'(x)_{\mathrm{data}})_{i,1}\big )_2 = \begin{cases} \mathcal{I}_1\segcc{I_i}(\sigma), &  \text{if }  I_i \in [\verb|constant_interval_expression|] \\ \big ((\sigma(I_i)_{\mathrm{data}})_{1,1}\big )_2, & \text{else if } I_i \in A \\ \mathbb{R}, & \text{otherwise} \end{cases};
// \end{equation*}
// %********************************************************
// %********************************************************
// \item
// $
// \mathcal{I}\llbracket x \verb| IN (| I_1,\ldots,I_p \verb|) TO | y \rrbracket (\sigma) = 
// \mathcal{I}\llbracket x \verb| IN (| I_1,\ldots,I_p \verb|) TO (| y \verb|)|\rrbracket (\sigma);
// $
// %********************************************************
// %********************************************************
// \item With $$w = \verb|(|x_1,\ldots,x_n \verb|) IN (| X_1,\ldots,X_p  \verb|) TO (| y_1,\ldots,y_m\verb|)|$$ and
// $X_i = I_{i,1}'$ or $X_i = \verb|(| I_{i,1}', \ldots, I_{i,k_i}'\verb|)|$ for all $i \in \intcc{1;p}$
// let
// \begin{equation*}
// \mathcal{I}\llbracket w \rrbracket (\sigma) = \begin{cases}
// \mathbf{error}, &\text{if } \sigma = \mathbf{error} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;n}} \sigma(x_i)_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, &\text{else if } \exists_{i,j \in \intcc{1;n}, i \neq j} \ x_i = x_j \\
// \mathbf{error}, &\text{else if } \exists_{(i,j) \in \intcc{1;n} \times \intcc{1;m} } \ x_i = \operatorname{name}\segcc{y_j} \\
// \mathbf{error}, &\text{else if } \exists_{i,j \in \intcc{1;m}, i \neq j} \ \operatorname{name}\segcc{y_i} = \operatorname{name}\segcc{y_j} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;m}} \sigma(\operatorname{name}\segcc{y_i})_{\mathrm{type}} \neq \mathbf{undeclared} \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;m}} \zeta \in \{\operatorname{row}_1\segcc{y_i}(\sigma),\operatorname{col}_1\segcc{y_i}(\sigma) \}  \\  
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;p},k_i=1} \forall_{k \in \intcc{1;n_\mathrm{max}}} \mathcal{I}_2\llbracket I_{i,j}' \rrbracket ( \sigma) \nsubseteq \mathbb{R}^k \\
// \mathbf{error}, &\text{else if } \exists_{i \in \intcc{1;p}} \exists_{j \in \intcc{1;k_i}} \mathcal{I}_2\llbracket I_{i,j}' \rrbracket ( \sigma) \nsubseteq \mathbb{R} \\
// \mathbf{error}, &\text{else if } n \neq p \\ 
// \sigma ', & \text{otherwise}
// \end{cases}
// \end{equation*}
// where $\sigma'$ coincides with $\sigma$ except that 
// \begin{flalign*}
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{idx}} = i,&& \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{type}} = \mathbf{Real}, \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{role}} = \mathbf{Output}, \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{rowdim}} = \operatorname{row}_1\segcc{y_i}(\sigma), \\
// &\forall_{i \in \intcc{1;m}} \ \sigma'(\operatorname{name}\segcc{y_i})_{\mathrm{coldim}} = \operatorname{col}_1\segcc{y_i}(\sigma), \\
// &\forall_{i \in \intcc{1;n}} \ \sigma'(x_i)_{\mathrm{idx}} = i, \\
// &\forall_{i \in \intcc{1;n}} \ \sigma'(x_i)_{\mathrm{type}} = \mathbf{Real}, \\
// &\forall_{i \in \intcc{1;n}} \ \sigma'(x_i)_{\mathrm{role}} = \mathbf{Input}, \\
// &\forall_{i \in \intcc{1;n}} \ \sigma'(x_i)_{\mathrm{coldim}} = 1, \\
// &\forall_{i \in \intcc{1;n}} \ \sigma'(x_i)_{\mathrm{rowdim}} = \begin{cases} \operatorname{dimension}\llbracket I_{i,1}' \rrbracket(\sigma), & \text{if } k_i = 1 \\ k_i, & \text{otherwise} \end{cases}
// \end{flalign*}
// and for all $i \in \intcc{1;n}$ 
// \begin{equation*}
// \footnotesize
// \forall_{j \in \intcc{1;l} } \ \big ((\sigma'(x_i)_{\mathrm{data}})_{j,1} \big )_2 = 
//\begin{cases} \mathcal{I}_1\llbracket I'_{i,j} \rrbracket(\sigma), &  \text{if }  I'_{i,j} \in [\verb|constant_interval_expression|] \\ 
// \big ((\sigma(I'_{i,j})_{\mathrm{data}})_{1,1}\big)_2, & \text{else if } I'_{i,j} \in A \text{ and } k_i>1 \\
// \big ((\sigma(I'_{i,1})_{\mathrm{data}})_{j,1}\big)_2, & \text{else if } I'_{i,1} \in A \text{ and } k_i=1 \\
// \mathbb{R}, & \text{otherwise} \end{cases},
// \end{equation*}
// where $l = \sigma'(x_i)_{\mathrm{rowdim}}$;
// %********************************************************
// %********************************************************
// \item 
// \label{l:fct:vi}
// \small
// \begin{equation*}
// \mathcal{I}\llbracket \verb|(|x_1,\ldots,x_n \verb|) IN (| X_1,\ldots,X_p \verb|) TO | y \rrbracket (\sigma) = 
// \mathcal{I}\llbracket \verb|(|x_1,\ldots,x_n \verb|) IN (| X_1,\ldots,X_p \verb|) TO (| y \verb|)|\rrbracket (\sigma);
// \end{equation*}
// \normalsize
// \end{enumerate}
// \end{semantics}
// \end{production}
// \subsubsection{Ordinary differential equations}
// The Backus-Naur forms in this section allow to represent functions of the form \ref{e:psi}. The central nonterminal is
// \verb|ode_definition| for which we need the following subset $S_\mathrm{ode}$ of states. 
// Define
// \begin{equation*}
// S_\mathrm{ode} = \{ \sigma \in S \mid \sigma \text{ satisfies } \ref{d:all} \} \cup \{ \mathbf{error}\} , \text{ where }
// \end{equation*}
// \begin{hypothesisA}
// \label{d:all}
// $0 < n \defas |\{ x \in A \mid \sigma(x)_\mathrm{role} \in \{ \mathbf{State}, \mathbf{OutputState}\}  \}| < \infty$, 
// $\{ x \in A \mid \sigma(x)_\mathrm{role} = \mathbf{Output} \}=\emptyset$,
// $0 < n' \defas |\{ x \in A \mid  \sigma(x)_\mathrm{role} = \mathbf{OutputState}\}|$,
// $0 \leq m \defas |\{ x \in A \mid  \sigma(x)_\mathrm{role} = \mathbf{InputConst} \}| < \infty$,
// $0 < m' \defas |\{ x \in A \mid  \sigma(x)_\mathrm{role} = \mathbf{Input} \}| < \infty$;
// there exists a unique $z \in A$ such that 
// the components $\mathrm{role}$ and $\mathrm{idx}$ of $\sigma(z)$ equal 
// $\mathbf{Time}$ and $t$, respectively;
// there exist $z_1,\ldots,z_{n} \in A$ such that for every $i \in \intcc{1;n}$ 
// the components $\mathrm{role}$, $\mathrm{rowdim}$ and $\mathrm{idx}$
// of $\sigma(z_i)$ equal $\mathbf{State}$ or $\mathbf{OutputState}$, $k_i$ and $i$, respectively;
// there exist $z_{n+1},\ldots,z_{n+m} \in A$ such that for every $i \in \intcc{n+1;n+m}$ 
// the components $\mathrm{role}$, $\mathrm{rowdim}$ and $\mathrm{idx}$
// of $\sigma(z_i)$ equal $\mathbf{InputConst}$, $k_i$ and $p_{i-n}>0$, respectively, and $p_{i-n} < p_{i+1-n}$ for every $i \in \intco{1;m}$;
// there exist $w_{1},\ldots,w_{m'} \in A$ such that for every $i \in \intcc{1;m'}$ 
// the components $\mathrm{role}$ and $\mathrm{idx}$
// of $\sigma(w_i)$ equal $\mathbf{Input}$ and $q_{i}>0$, respectively, and $q_i < q_{i+1}$ for every $i \in \intco{1;m'}$.
// \end{hypothesisA}
// Loosely speaking, $\sigma \in S_\mathrm{ode}$ contains a representation of an ordinary differential equation
// of the form \ref{e:ode} (if $m>0$) or \ref{e:ode:2} (if $m=0$) and a representation of a function of the form \ref{e:psi}. (Details are given in Section \ref{ss:auxode}.)
// \begin{production}
//\begin{verbatim}
ode_definition
	: function_declaration function_head ode_statement           { $$ = $3 ; }
	;
//\end{verbatim}
// \begin{semantics}
// Let $a_i \in N_i$, $i \in \{1,2,3\}$, where \vspace{.3cm}\\
// \begin{tabular}{ll}
// &$N_1 = \verb|function_declaration|$, \\
// &$N_2 = \verb|function_head|$, \\
// &$N_3 = \verb|ode_statement|$, \\
// \end{tabular}
// ~\vspace{.3cm}\\
// and let $\mathcal{I}_i$ be the interpretation function for $N_i$, $i \in \{1,2,3\}$. Define 
// \[\mathcal{I} \colon [\verb|ode_definition|] \to \{ S \to S \times S \}\] for all $\sigma \in S$ by
// \begin{equation*}
// \small
// \mathcal{I}\segcc{a_1 \ a_2 \ a_3}(\sigma) = \begin{cases} 
// (\mathbf{error},\mathbf{error}), & \text{if } \sigma = \mathbf{error} \\
// (\mathbf{error},\mathbf{error}), & \text{else if } \mathbf{error} \in \{ \sigma_\mathrm{dom},\sigma_\mathrm{final}\} \\
// (\mathbf{error},\mathbf{error}), & \text{else if } \sigma_\mathrm{final} \notin S_\mathrm{ode} \\
// (\sigma_\mathrm{dom},\sigma_\mathrm{final}), & \text{otherwise} \end{cases}
// \end{equation*}
// where $\sigma_\mathrm{dom} = \mathcal{I}_2\segcc{a_2} \big ( \mathcal{I}_1\segcc{a_1}(\sigma) \big )$, $\sigma_\mathrm{final} = \mathcal{I}_3\segcc{a_3}(\sigma_\mathrm{dom},\sigma)$.
// \end{semantics}~\\
// \begin{remarks}
// We illustrate below previous compositions of the interpretation functions:
// \input{tikzpic.tex}
// \end{remarks}
// \end{production}
// \begin{production}
//\begin{verbatim}
ode_statement
	: '{' ode_equation ';' initialvalue_list '}'  { $$ = build_ode_statement($2,$4); }
	| '{' initialvalue_list ode_equation ';' '}'  { $$ = build_ode_statement($3,$2); }
	;
//\end{verbatim}
// \begin{semantics}
// Let $a \in N_1$, $b \in N_2$, where $N_1 = \verb|ode_equation|$ and $N_2 = \verb|initialvalue_list|$.
// Let $\mathcal{I}_i$ be the interpretation function
// for $N_i$, $i \in \{1,2\}$.
// Define $$\mathcal{I} \colon [\verb|ode_statement|] \to \{ S \times S \to S \}$$ for all $\sigma_\mathrm{dom},\sigma \in S$ by
// \begin{equation*}
// \mathcal{I}\llbracket \verb|{| b \ a  \verb|;}| \rrbracket(\sigma_\mathrm{dom},\sigma) = \mathcal{I}\llbracket \verb|{| a \verb|;| b  \verb|}| \rrbracket(\sigma_\mathrm{dom},\sigma) = \mathcal{I}_2\segcc{b}(\mathcal{I}_1 \segcc{a} (\sigma_\mathrm{dom},\sigma) ) .
// \end{equation*}
// \end{semantics}
// \end{production}
// \begin{production}
// \small
// \begin{verbatim}
ode_equation
	: DIFF '(' identifier       ',' identifier ')' '=' identifier identifier_tuple  { $$ = install_ode_statement($3,$5,$8,$9); }
	| DIFF '(' identifier_tuple ',' identifier ')' '=' identifier identifier_tuple  { $$ = install_ode_statement($3,$5,$8,$9); }
	;
// \end{verbatim}
// \normalsize
// \begin{semantics}
// Let $f,x_1,\ldots,x_n,y_1,\ldots,y_m,t \in [\verb|identifier|]$. \\
// Define $$\mathcal{I} \colon [\verb|ode_equation|] \to \{ S \times S \to S\}$$ for all $\tau\defas \sigma_\mathrm{dom},\sigma \in S$
// \begin{enumerate}
// \item with $w = \verb|DIFF(|x_1,t\verb|) = |f \verb|(|y_1,\ldots,y_m\verb|)|$, respectively,
// \item with $w = \verb|DIFF((|x_1,\ldots,x_n \verb|),|t\verb|) = |f \verb|(|y_1,\ldots,y_m\verb|)|$
// \end{enumerate}
// by
// \begin{equation*}
// \mathcal{I}\llbracket w \rrbracket  (\tau,\sigma) = \begin{cases}
// \mathbf{error}, & \text{if } \mathbf{error} \in \{ \tau,\sigma\} \\
// \mathbf{error}, & \text{else if } \tau \notin S_\mathrm{dom} \\
// \mathbf{error}, & \text{else if } \tau(f)_\mathrm{type} \neq \mathbf{Function} \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, this case is required to ensure that the explicitly defined function in a program is used in $w$ with the correct number of image components.} \ |\{ z \in A \mid  \tau(z)_\mathrm{role} = \mathbf{Output} \}|  \neq n \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, this case is required to ensure that the explicitly defined function in a program is used in $w$ with the correct number of arguments.} \ |\{ z \in A \mid  \tau(z)_\mathrm{role} = \mathbf{Input} \}|  \neq m \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, this and the next case are required to ensure that the explicitly defined function in a program can be used as a right hand side in \ref{e:ode} or \ref{e:ode:2}.} \ n > m \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, the $i$-th argument of $f$ shall be of the same dimension as the $i$-th image component of $f$.} \ \exists_{i \in \intcc{1;n}}\exists_{y,z\in A} (\tau(y)_\mathrm{role},\tau(y)_\mathrm{idx}) = (\mathbf{Output},i) \\
// &\text{and } (\tau(z)_\mathrm{role},\tau(z)_\mathrm{idx}) = (\mathbf{Input},i) \\
// &\text{and } (\tau(y)_\mathrm{rowdim}  \neq \tau(z)_\mathrm{rowdim} \text{ or } \{\tau(y)_\mathrm{coldim},\tau(z)_\mathrm{coldim}\}  \neq \{1\}) \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, this and the next seven cases are required to ensure that an ordinary differential equation of the form \ref{e:ode} or \ref{e:ode:2} is represented in $w$.} \  \exists_{i \in \intcc{n+1;m} } \ \sigma(y_i)_\mathrm{role} \neq \mathbf{Input} \\
// \mathbf{error}, & \text{else if } \exists_{i \in \intcc{n+1;m} }\exists_{z \in A} \ \sigma(y_i)_\mathrm{rowdim}  \neq \tau(z)_\mathrm{rowdim}, \\
// & \text{and } (\tau(z)_\mathrm{role},\tau(z)_\mathrm{idx}) = (\mathbf{Input},i) \\
// \mathbf{error}, & \text{else if } \exists_{i \in \intcc{1;n} } \ x_i \neq y_i \\
// \mathbf{error}, & \text{else if } \exists_{i,j \in \intcc{1;m}, i \neq j } \ y_i = y_j \\
// \mathbf{error}, & \text{else if } \exists_{j \in \intcc{1;m} } \ y_i = t \\
// \mathbf{error}, & \text{else if } \exists_{i \in \intcc{1;n} } \ \sigma(y_i)_\mathrm{role} \neq \mathbf{Output} \\
// &\text{and } \sigma(y_i)_\mathrm{type} \neq \mathbf{undeclared} \\
// \mathbf{error}, & \text{else if } \exists_{i \in \intcc{1;n} } \exists_{z \in A} \ \sigma(y_i)_\mathrm{role} = \mathbf{Output} \\
// &\text{and } (\sigma(y_i)_\mathrm{rowdim}  \neq \tau(z)_\mathrm{rowdim} \text{ or } \{\sigma(y_i)_\mathrm{coldim},\tau(z)_\mathrm{coldim}\}  \neq \{1\}) , \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, this case is required to ensure a correctly specified independent variable (time) for the ordinary differential equation.} \ \sigma(t)_\mathrm{role} \neq \mathbf{Input} 
//  \text{ or } \{\sigma(t)_\mathrm{rowdim},\sigma(t)_\mathrm{coldim}\} \neq \{1\} \\
//  & \text{or } ((\sigma(t)_{\mathrm{data}})_{1,1})_2 \text{ is not compact}  \\
// \sigma ' , & \text{otherwise}
// \end{cases}
// \end{equation*}
// where $\sigma ' $ coincides with $\sigma$ except that 
// \begin{align*}
// &\sigma'(t)_\mathrm{role} = \mathbf{Time}, \\
// &\big ( (\sigma'(t)_{\mathrm{data}})_{1,1} \big )_2 = \conv\{ \{0\}, \big ((\sigma(t)_{\mathrm{data}})_{1,1} \big )_2 \} , \\
// \forall_{i \in \intcc{1;n}} \ &\sigma'(y_i)_\mathrm{idx} = i, \\
// \forall_{i \in \intcc{1;n}} \ &\sigma'(y_i)_{\mathrm{type}} = \mathbf{Real}, \\
// \forall_{i \in \intcc{1;n}} \ &\sigma'(y_i)_\mathrm{role} = 
// \begin{cases} 
// \mathbf{OutputState}, & \text{if } \sigma(y_i)_\mathrm{role} = \mathbf{Output}\\
// \mathbf{State}, & \text{otherwise}
// \end{cases}, \\
// \forall_{i \in \intcc{n+1;m}} \ &\sigma'(y_i)_\mathrm{role} = \mathbf{InputConst}, \\
// \forall_{i \in \intcc{1;n}} \ &\sigma'(y_i)_\mathrm{rowdim} = \tau(z)_\mathrm{rowdim}, \text{ where } (\tau(z)_\mathrm{role},\tau(z)_\mathrm{idx}) = (\mathbf{Input},i), \\
// \forall_{i \in \intcc{1;n}} \ &\sigma'(y_i)_\mathrm{coldim} = 1.
// \end{align*}
// \end{semantics}
// \end{production}
// Using the next Backus-Naur forms, the function $g$ in \ref{e:psi} can be represented, and therefore the image $\psi(0,y_1,\ldots,y_k,u_1,\ldots,u_m)$. We recall that $g$ shall satisfy $\im g \subseteq X_0 \subseteq \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}$. 
// \begin{production}
// \begin{verbatim}
initialvalue
	: INITIALVALUE '(' postfix_expression ',' expression ')' ';' { $$ = install_statement(DEFI,(void *)install_assignment(1,$3,$5)); }
	;
//\end{verbatim}
// \begin{semantics}
// Let $\mathcal{I}_1$ be the interpretation function for $\verb|definition|$. 
// Let $a \in A_1 = [\verb|postfix_expression|]$. Then 
// $$a = x \ \text{ or } \ a = x\verb|[|E\verb|]| \ \text{ or } \ a = x\verb|[|E\verb|][|E'\verb|]|$$ 
// for some $x \in [\verb|identifier|]$, $E,E' \in [\verb|expression|]$. Let $E'' \in [\verb|expression|]$.\\
// There is the following difference between cases \ref{initialvalue:i} and \ref{initialvalue:ii} below. Let the notation be as in \ref{e:solution},\ref{e:psi}. 
// Case \ref{initialvalue:i} allows to define $g(y_1,\ldots,y_k)_i = y_j$ 
// by specifying $i \in \intcc{1;n}$ and $j \in \intcc{1;k}$. 
// Case \ref{initialvalue:ii} allows to define the $j$th component of $g(y_1,\ldots,y_k)_i$ explicitly for
// $i \in \intcc{1;n}$, $j \in \intcc{1;d_i}$. 
// \\
// Define 
// $$\mathcal{I} \colon [\verb|initialvalue|] \to \{S \to S\}$$ for all $\sigma \in S$ by
// \begin{enumerate}
// \item
// \label{initialvalue:i}
// \begin{equation*}
// \footnotesize
// \mathcal{I}\llbracket \verb|INITIALVALUE(| x \verb|,| E'' \verb|);| \rrbracket (\sigma) = 
// \begin{cases}
// \mathbf{error}, & \text{if }      \sigma = \mathbf{error} \text{ or } \sigma \notin S_\mathrm{ode} \\
// \mathbf{error}, & \text{else if } \sigma(x)_\mathrm{type} \neq \mathbf{Real} \text{ or } \sigma(x)_\mathrm{coldim} \neq 1 \\
// \mathbf{error}, & \text{else if } \sigma(x)_\mathrm{role} \notin \{ \mathbf{State},\mathbf{OutputState}\} \\
// \mathbf{error}, & \text{else if } E'' \in A_1 \text{ and } (\sigma(\operatorname{name}\segcc{E''})_\mathrm{type} \neq \mathbf{Real} \\
// &\text{or } \sigma(\operatorname{name}\segcc{E''})_\mathrm{role} \notin \{ \mathbf{Ordinary},\mathbf{Input}\}) \\
// \mathbf{error}, & \text{else if}\footnote{Loosely speaking, the dimensions of identifiers $x$ and $E''$ shall coincide. A similar remark holds for the next case. } \ E'' \in A \text{ and } \\
// &(\sigma(x)_{\mathrm{rowdim}} \neq \sigma(E'')_{\mathrm{rowdim}} \text{ or }  \sigma(E'')_{\mathrm{coldim}} \neq 1 )\\
// \mathbf{error}, & \text{else if } E'' \notin A \text{ and } \sigma(x)_{\mathrm{rowdim}} \neq 1  \\
// \sigma', & \text{else if } E'' \notin A \text{ and } \sigma(x)_{\mathrm{rowdim}} = 1 \\
// \sigma_n,        & \text{otherwise}
// \end{cases},
// \end{equation*}
// where $\sigma ' = \mathcal{I}_1 \llbracket x\texttt{=}E''\texttt{;}\rrbracket (\sigma)$ and
// \begin{equation*}
// \forall_{i \in \intcc{0;n-1}} \ \sigma_{i+1} = 
// \mathcal{I}_1 \llbracket x \texttt{[} i \texttt{]=} E''\texttt{[} i \texttt{]} \texttt{;} \rrbracket (\sigma_{i}) 
// \end{equation*}
// where $\sigma_0=\sigma$, 
// $n = \sigma(x)_{\mathrm{rowdim}}$.
// \item
// \label{initialvalue:ii}
// \begin{equation*}
// \footnotesize
// \mathcal{I}\llbracket \verb|INITIALVALUE(| x \verb|[|E\verb|],| E'' \verb|);| \rrbracket (\sigma) = 
// \begin{cases}
// \mathbf{error}, & \text{if }      \sigma = \mathbf{error} \text{ or } \sigma \notin S_\mathrm{ode} \\
// \mathbf{error}, & \text{else if } \sigma(x)_\mathrm{type} \neq \mathbf{Real} \text{ or } \sigma(x)_\mathrm{coldim} \neq 1 \\
// \mathbf{error}, & \text{else if } \sigma(x)_\mathrm{role} \notin \{ \mathbf{State}, \mathbf{OutputState} \} \\
// \mathbf{error}, & \text{else if } E'' \in A_1 \text{ and } (\sigma(\operatorname{name}\segcc{E''})_\mathrm{type} \neq \mathbf{Real} \\
// &\text{or } \sigma(\operatorname{name}\segcc{E''})_\mathrm{role} \notin \{ \mathbf{Ordinary},\mathbf{Input}\}) \\
// \sigma', & \text{otherwise}
// \end{cases},
// \end{equation*}
// where $\sigma ' = \mathcal{I}_1\llbracket x \verb|[|E\verb|]=| E'' \verb|;| \rrbracket (\sigma)$.
// \item
// \label{initialvalue:iii}
// $\mathcal{I}\llbracket \verb|INITIALVALUE(| x \verb|[|E\verb|][|E'\verb|],| E'' \verb|);| \rrbracket (\sigma) = \mathbf{error}$.
// \end{enumerate}
// \end{semantics}
// \begin{remarks}
// For example, the program in \ref{fig:1} specifies $g$ in \ref{e:psi} as the identity map on $\intcc{0,2\pi} \times \intcc{-1,1}$. If we replaced line 14 with $$\verb|initialvalue(x[0],x0[0]); initialvalue(x[1],x0[1]^2);|$$ then $g$ would be given by $(x_1,x_2) \mapsto (x_1,x_2^2)$. Similarly, the last but one line in \ref{fig:2} defines $g(a,b)_2 = -1 + b$. 
// \end{remarks}
// \end{production}
// \begin{production}
//\begin{verbatim}
initialvalue_list 
	: initialvalue                             { $$ = $1 ; }
	| initialvalue_list initialvalue           { $$ = (Statement)append(AST_STMT,(void *)$1,(void *)$2); }
	;
//\end{verbatim}
// \begin{semantics}
// Let $a \in  [ \verb|initialvalue|]$, $b \in [\verb|initialvalue_list|]$. 
// Let $\mathcal{I}_1$ be the interpretation function for $\verb|initialvalue|$. 
// Define $$\mathcal{I} \colon [\verb|initialvalue_list|] \to \{S \to S\}$$ for all $\sigma \in S$ by
// \begin{enumerate}
// \item $\mathcal{I}\segcc{a}(\sigma) = \mathcal{I}_1\segcc{a}(\sigma)$,
// \item $\mathcal{I}\segcc{b \ a}(\sigma) = \mathcal{I}_1\segcc{a}(\mathcal{I}\segcc{b}(\sigma))$.
// \end{enumerate}
// \end{semantics}
// \end{production}
// \subsubsection{Options}
// None of the Backus-Naur forms in this section imply a semantic
// meaning
// but may be considered parameters to be 
// passed to compilers to be defined later.
// \begin{production}
//\begin{verbatim}
/* Constraints:
 * 1) The identifier in position 9 shall be declared as 'Function'
 * 2) All identifiers in position 4 shall appear in position 10
 * 3) At least one identifier in position 4 shall appear in position 5 of function_head
*/
options
	: option_list ';'          { $$ = $1 ; }
	| option_list              { $$ = $1 ; }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
option_list
	: option                           { $$ = $1 ; }
	| option_list ';' option  		{ $$ = (Option)append(AST_OPT,(void *)$1,(void *)$3) ;     }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
option
	: option_head option_statement { $$ = current_opt ; }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
option_head
	: OPTION postfix_expression ':' 	  { current_opt = install_option($2)   ;          }
	| OPTION ':'					  { current_opt = install_option(NULL) ;          }
	;
//\end{verbatim}
// \end{production}
// \begin{production}
//\begin{verbatim}
option_statement
	: ORDER '(' identifier ',' expression ')' { $$ = build_option(OPTION_ORDER,current_opt,$3,$5); }
	;
//\end{verbatim}
// \end{production}
// \subsubsection{Auxiliary functions related to function definitions}
// \label{ss:auxfunctions}
// We define below auxiliary functions $\mathcal{D}$ and $\mathcal{F}$, which we will use in the next sections.
// Intuitively speaking, the function $\mathcal{D}$ maps $\sigma \in S_\mathrm{dom}$ 
// to the domain of a function specified in the program with ``current" state $\sigma$. (Recall that a program defines 
// a finite sequence in $S$.)
// Formally, define $\mathcal{D}$ on $S_\mathrm{dom}$ by
// \begin{align*}
// %\mathcal{D} \colon S_\mathrm{dom} &\to \Big ( \bigcup_{j \in \mathbb{N}} \big ( \bigcup_{i \in \mathbb{N}} I_\mathbb{R}^i \big )^j \Big ) \cup \{\zeta\} \\
// %\sigma &\mapsto 
// \mathcal{D}(\sigma) = 
// \begin{cases}
// \zeta, & \text{if } \sigma = \mathbf{error} \\
// M_1 \times \ldots \times M_n, & \text{otherwise}
// \end{cases}
// \end{align*}
// where $n$, $x_1,\ldots,x_n$, $k_1,\ldots,k_n$ as in \ref{d:dom} for $\sigma$ and
// \begin{align*}
// &\forall_{i \in \intcc{1;n}} \ M_i = \big ( (\sigma(x_i)_{\mathrm{data}})_{1,1} \big )_2 \times \ldots \times \big ( (\sigma(x_i)_{\mathrm{data}})_{k_i,1} \big )_2.
// \end{align*}
// %
// %
// Next, the image of $\mathcal{F}$ is the explicitly defined function in the program. 
// Define
// \begin{equation*}
// \label{e:fct}
// \mathcal{F} \colon  S_\mathrm{fct}  \to \bar{F} \cup \{ \zeta \}
// \end{equation*}
// by $\mathcal{F}(\sigma) = \zeta$ if $\sigma = \mathbf{error}$ and otherwise define
// $\mathcal{F}(\sigma)$ as
// \begin{align*}
// \mathcal{F}(\sigma) \colon \mathcal{D}(\sigma)&\to \overline{\mathbb{R}}^{k_1 \times l_1} \times \ldots \times \overline{\mathbb{R}}^{k_m \times l_m}  \\
// (x_1,\ldots,x_n) &\mapsto (Y^{(1)}(x_1,\ldots,x_n), \ldots, Y^{(m)}(x_1,\ldots,x_n))
// \end{align*}
// where $n$ as in \ref{d:dom} for $\sigma$, $m,y_1,\ldots,y_m$ as in \ref{d:fct} for $\sigma$ and
// \begin{align*}
// &\forall_{\nu \in \intcc{1;m}} \forall_{(i,j) \in \intcc{1;k_\nu} \times \intcc{1;l_\nu} } \ Y^{(\nu)}_{i,j} = \big ( (\sigma(y_\nu)_{\mathrm{data}})_{i,j} \big )_1, \text{ where }
// k_\nu = \sigma(y_\nu)_\mathrm{rowdim}, 
// l_\nu = \sigma(y_\nu)_\mathrm{coldim}.
// \end{align*}
// \subsubsection{Auxiliary functions related to ordinary differential equations }
// \label{ss:auxode}
// Below, we define functions $\mathcal{S}_\mathrm{all}$ and $\mathcal{S}_\mathrm{part}$ 
// which we will use for defining $\mathcal{I}_\mathrm{prog}$ in the next section. 
// First, we specify the class of right hand sides for \ref{e:ode} and \ref{e:ode:2} to which
// we restrict ourselves. Let 
// \begin{align}
// \nonumber
// F_{\mathrm{rhs},D}^\mathrm{i} = \{ f \colon X \times U &\to Y | \exists_{n \in \mathbb{N}} \exists_{d_1,\ldots,d_n \in \mathbb{N}} \ \emptyset \neq X \subseteq \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n},
// \\ \nonumber &\exists_{m \in \mathbb{N}} \exists_{d_{n+1},\ldots,d_{n+m} \in \mathbb{N}} \ \emptyset \neq U \subseteq \mathbb{R}^{d_{n+1}} \times \ldots \times \mathbb{R}^{d_{n+m}}, 
// \\ \nonumber &Y = \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}, \text{ and } 
// \\ \nonumber &\forall_{u \in U} f(\cdot,u) \text{ is extendable to some neighborhood } \tilde X \text{ of } X,
// \\ \label{e:rhs:i} & \forall_{u \in U} f(\cdot,u) \text{ is of class } C^\infty \text{ on } \tilde X \},
// \end{align}
// \begin{align}
// \nonumber
// F_{\mathrm{rhs},D}^\mathrm{a} =  \{ f \colon X \to Y &| \exists_{n \in \mathbb{N}} \exists_{d_1,\ldots,d_n \in \mathbb{N}} \ \emptyset \neq X \subseteq \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}, 
// \\ \nonumber &Y = \mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n} \text{ and } 
// \\ \nonumber &f\text{ is extendable to some neighborhood } \tilde X \text{ of } X,
// \\ \label{e:rhs:a} & f \text{ is of class } C^\infty \text{ on } \tilde X \},
// \end{align}
// and let $F_{\mathrm{rhs}}^\mathrm{i}$ and $F_{\mathrm{rhs}}^\mathrm{a}$ be defined by \ref{e:rhs:i} and \ref{e:rhs:a}, 
// respectively, but with the extendability and differentiability condition removed and $Y = \overline{\mathbb{R}}^{d_1} \times \ldots \times \overline{\mathbb{R}}^{d_n}$ in place of $Y=\mathbb{R}^{d_1} \times \ldots \times \mathbb{R}^{d_n}$.
// \\
// Below, we consider only $F_\mathrm{rhs}^\mathrm{i}$ when defining $\mathcal{S}_\mathrm{all}$ and $\mathcal{S}_\mathrm{part}$. 
// The definitions for $F_\mathrm{rhs}^\mathrm{a}$ are similar. 
// Intuitively speaking, the image of $\mathcal{S}_\mathrm{all}$ 
// is a solution of the ordinary differential equation \ref{e:ode} with
// $f$ given by the third argument to $\mathcal{S}_\mathrm{all}$. The first argument to $\mathcal{S}_\mathrm{all}$ 
// specifies $I$ and $U_0$, and the second argument $X_0$ in \ref{e:solution}. 
// The rigorous definitions are as follows. \\
// Define
// $\mathcal{S}_\mathrm{all}$ on $S_\mathrm{dom} \times S_\mathrm{ode} \times F_{\mathrm{rhs}}^\mathrm{i}$
// by $\mathcal{S}_\mathrm{all}(\sigma,\tau,f) = \zeta$ if $\mathbf{error} \in \{ \sigma,\tau\}$ or $\varphi$ as below
// does not exist, otherwise define $\mathcal{S}_\mathrm{all}(\sigma,\tau,f)=\varphi$.
// Define $\varphi$ by \ref{e:solution} satisfying 
// \begin{align*}
// \im \varphi \times U_0 &\subseteq \dom f, \\
// f|_{\im \varphi \times U_0} &\in F_{\mathrm{rhs},D}^\mathrm{i},\\
// f|_{\im \varphi \times U_0} &\subseteq \mathbb{R}^{k_1} \times \ldots \times \mathbb{R}^{k_n}, \\
// \forall_{(t,x,u) \in I \times X_0 \times U_0} D_1\varphi(t,x,u) &= f(\varphi(t,x,u),u) \text{ and } \varphi(0,x,u) = x, 
// \end{align*}
// where
// \begin{align*}
// &n,m,z,z_1,\ldots,z_{n+m},k_1,\ldots,k_{n+m} \text{ as in } \ref{d:all} \text{ with } \tau \text{ in place of } \sigma \text{ and we assume } m>0, \\
// &I = \big ( (\sigma(z)_{\mathrm{data}})_{1,1} \big )_2, \\
// &X_0 = M_1 \times \ldots \times M_n, \text{ where }
// \forall_{i \in \intcc{1;n}} M_i = \big ( (\tau(z_i)_{\mathrm{data}})_{1,1} \big )_1(\mathcal{D}(\tau)) \times \ldots \times \big ( (\tau(z_i)_{\mathrm{data}})_{k_i,1} \big )_1(\mathcal{D}(\tau)), \\
// &U_0 = M_{n+1} \times \ldots \times M_{n+m}, \text{ where }
// \forall_{i \in \intcc{n+1;n+m}} M_i = \big (  (\sigma(z_i)_{\mathrm{data}})_{1,1} \big )_2 \times \ldots \times \big ( (\sigma(z_i)_{\mathrm{data}})_{k_i,1} \big )_2. 
// \end{align*}
// Note that $\mathcal{D}(\tau)$ is well-defined as \ref{d:all} implies \ref{d:dom}, and that the existence of $\varphi$ can be verified through 
// $f$ and $I \times X_0 \times U_0$ \cite[Prop.~2.1]{GriewankWalther08},\cite{Lohner88}. \par
// Next, intuitively speaking, the image of $\mathcal{S}_\mathrm{part}$ is the function in \ref{e:psi}. Define
// $\iota_\tau$ for $\tau \in S_\mathrm{ode}$ as the map that identifies an element of 
// $\bar F$ with one of $F_\mathrm{rhs}^\mathrm{i}$ in the obvious way specified by $\tau(\cdot)_\mathrm{role}$ if feasible otherwise $\iota_\tau$ maps the element to $\zeta$.
// Define 
// \begin{equation*}
// \mathcal{S}_\mathrm{part}\colon S_\mathrm{dom} \times S_\mathrm{ode} \times \bar{F} \to F \cup \{ \zeta \} 
// \end{equation*}
// by $\mathcal{S}_\mathrm{part} (\sigma,\tau,f) = \zeta$ if $\mathbf{error} \in \{\sigma,\tau\}$ or $\iota_\tau(f) = \zeta$ or
// $\mathcal{S}_\mathrm{all}(\sigma,\tau,\iota_\tau(f)) = \zeta$ and 
// otherwise as $\mathcal{S}_\mathrm{part}(\sigma,\tau,f) = \psi$, where $\psi$ is defined as follows. Let
// $n,n',m,m',t,z_1,\ldots,z_n$, $k_1,\ldots,k_n$, $p_1,\ldots,p_m,q_1,\ldots,q_{m'}$ be as in \ref{d:all} with $\tau$ in place of $\sigma$, and 
// \begin{align*}
// \psi \colon \mathcal{D}(\sigma) &\to \mathbb{R}^{k_{l_1}} \times \ldots \times \mathbb{R}^{k_{l_{n'}}} \\
// x & \mapsto \big ( \varphi_{l_1}, \ldots, \varphi_{l_{n'}} \big )(x_t,(Y^{(1)}(x_{q_1},\ldots,x_{q_{m'}}),\ldots,Y^{(n)}(x_{q_1},\ldots,x_{q_{m'}})),(x_{p_1},\ldots,x_{p_m}))
// \end{align*} 
// %
// where
// \begin{align*}
// & \varphi = \mathcal{S}_\mathrm{all}(\sigma,\tau,\iota_\tau(f)), \\
// &\forall_{j \in \intcc{1;n}} \forall_{i \in \intcc{1;k_j} } \ Y^{(j)}_i = \big ( (\sigma(z_j)_{\mathrm{data}})_{i,1} \big )_1, \\
// &\forall_{i \in \intcc{1;n'}} \exists_{y \in A} \ (\tau(y)_\mathrm{role},\tau(y)_\mathrm{idx} ) = ( \mathbf{OutputState},l_i ).
// \end{align*}
// \subsubsection{Start symbol}
// \label{ss:startsymbol}
// The start symbol of $G$ is the nonterminal \verb|program|. 
// \begin{production}
// \begin{verbatim}
program		        
	: function_definition                                       { AST = install_program(NULL,$1,NULL,NULL) ; }
	| function_definition options                               { AST = install_program(NULL,$1,NULL,$2) ;   }
	| function_definition ode_definition                        { AST = install_program(NULL,$1,$2,NULL) ;   }
	| function_definition ode_definition options                { AST = install_program(NULL,$1,$2,$3) ;     }
	| statement_list function_definition                        { AST = install_program($1,$2,NULL,NULL) ;   }
	| statement_list function_definition options                { AST = install_program($1,$2,NULL,$3) ;     }
	| statement_list function_definition ode_definition         { AST = install_program($1,$2,$3,NULL) ;     }
	| statement_list function_definition ode_definition options { AST = install_program($1,$2,$3,$4) ;       }
	;
// \end{verbatim}
// \begin{semantics}
// Let $a_i \in [N_i]$, $i \in \intcc{1;4}$, where 
// \vspace{.3cm}\\
// \begin{tabular}{ll}
// &$N_1 = \verb|statement_list|$, \\
// &$N_2 = \verb|function_definition|$, \\
// &$N_3 = \verb|ode_definition|$, \\
// &$N_4 = \verb|options|$. 
// \end{tabular}
// \vspace{.3cm}\\
// Let $\mathcal{I}_i$ be the interpretation
// function for $N_i$, $i \in \{1,2,3\}$. Let $\sigma_0 \in S$ be
// the initial state of the variables defined in \ref{e:initialstate}. 
// ($\sigma_\mathrm{final} \in S$ defined below is to be considered the state 
// after the last writing instruction in the program, 
// $\sigma_\mathrm{dom} \in S$ the state right before the writing instructions produced from \verb|ode_statement|.)
// Define
// $$ \mathcal{I}_\mathrm{prog} \colon [\verb|program|] \to F \cup \{\zeta\}$$
// by 
//
// \begin{enumerate}
// \item \label{prog:i} \[\mathcal{I}_\mathrm{prog} \segcc{a_2}  = \begin{cases} \zeta, & \text{if } \mathcal{F}(\mathcal{I}_2\segcc{a_2}(\sigma_0)) \notin F \\
// \mathcal{F}(\mathcal{I}_2\segcc{a_2}(\sigma_0)) & \text{otherwise} \end{cases} ;
// \]
// \item \label{prog:ii} $\mathcal{I}_\mathrm{prog} \segcc{a_2 \ a_4}  = \mathcal{I}_\mathrm{prog} \segcc{a_2}$;
// \item \label{prog:iii} 
// \begin{equation*}
// \mathcal{I}_\mathrm{prog} \segcc{a_2 \ a_3}  = \begin{cases} 
// \zeta, & \text{if } \mathcal{F}(\mathcal{I}_2\segcc{a_2}(\sigma_0))=\zeta  \\
// \psi, & \text{otherwise} \end{cases},
// \end{equation*}
// where 
// \begin{align*}
// &\psi = \mathcal{S}_{\mathrm{part}}\big (\sigma_\mathrm{dom},\sigma_\mathrm{final},\mathcal{F}(\mathcal{I}_2\segcc{a_2}(\sigma_0)) \big ), \\
// &(\sigma_\mathrm{dom},\sigma_\mathrm{final}) = \mathcal{I}_3 \segcc{a_3}\big ( \mathcal{I}_2\segcc{a_2}(\sigma_0)\big) \in S \times S;
// \end{align*}
// \item \label{prog:iv} $\mathcal{I}_\mathrm{prog} \segcc{a_2 \ a_3 \ a_4} = \mathcal{I}_\mathrm{prog} \segcc{a_2 \ a_3}$;
// \item \label{prog:v} \[\mathcal{I}_\mathrm{prog} \segcc{a_1 \ a_2}  = \begin{cases} \zeta, & \text{if }  \mathcal{F}\big (\mathcal{I}_2\segcc{a_2}(\mathcal{I}_1\segcc{a_1}(\sigma_0)) \big ) \notin F \\ \mathcal{F}\big (\mathcal{I}_2\segcc{a_2}(\mathcal{I}_1\segcc{a_1}(\sigma_0)) \big ), & \text{otherwise} \end{cases};\]
// \item \label{prog:vi} $\mathcal{I}_\mathrm{prog} \segcc{a_1 \ a_2 \ a_4}  = \mathcal{I}_\mathrm{prog} \segcc{a_1 \ a_2}$;
// \item \label{prog:vii} $$\small\mathcal{I}_\mathrm{prog} \segcc{a_1 \ a_2 \ a_3}  = \begin{cases} 
// \zeta, & \text{if } \mathcal{F}\big (\mathcal{I}_2\segcc{a_2}(\mathcal{I}_1\segcc{a_1}(\sigma_0)) \big )=\zeta  \\
// \psi , & \text{otherwise} \end{cases}, $$
// where 
// \begin{align*}
// &\psi = \mathcal{S}_{\mathrm{part}}\big(\sigma_\mathrm{dom},\sigma_\mathrm{final},\mathcal{F}(\mathcal{I}_2\segcc{a_2}(\mathcal{I}_1\segcc{a_1}(\sigma_0))  ) \big ),\\
// &(\sigma_\mathrm{dom},\sigma_\mathrm{final}) = \mathcal{I}_3 \segcc{a_3}\big ( \mathcal{I}_2\segcc{a_2}(\mathcal{I}_1 \segcc{a_1}(\sigma_0)) \big ) \in S \times S; \\
// \end{align*}
// \item \label{prog:viii} $\mathcal{I}_\mathrm{prog} \segcc{a_1 \ a_2 \ a_3 \ a_4} = \mathcal{I}_\mathrm{prog} \segcc{a_1 \ a_2 \ a_3}$.
// \end{enumerate}
// \begin{remarks}
// By the definitions of the interpretation functions in Sections \ref{s:syntax} it follows that
// \begin{align*}
// &\mathcal{I}_2\segcc{a_2}(\sigma_0) \in S_\mathrm{fct}, \\
// &\mathcal{I}_2\segcc{a_2}(\mathcal{I}_1\segcc{a_1}(\sigma_0)) \in S_\mathrm{fct},
// \end{align*}
// and $(\sigma_\mathrm{dom},\sigma_\mathrm{final}) \in S_\mathrm{dom} \times S_\mathrm{ode}$ 
// in cases \ref{prog:iii},\ref{prog:iv} and \ref{prog:vii},\ref{prog:viii}, respectively. See also the remark to the Backus-Naur form defining
// $\verb|ode_definition|$.
// \end{remarks}
// \end{semantics}
// \end{production}
/****************************************************************
 *                                                              *
 *    End of grammar specification                              *
 *                                                              *
 ****************************************************************/
%%
void yyerror(char s[])
{
   extern int yylineno;
   extern char yytext[];
   fprintf(stderr,"Line %d near '%s':  %s\n", yylineno, yytext, s);
   exit(EXIT_FAILURE);
}
/****************************************************************
 * Changes to Version 01/02/2016:
 * 1) "-x^2" is now understood as "-(x*x)" and not as (-x)^2.
 *2) The non-terminal "index" is introduced to make documentation
 *   more intuitive.
 * 3) Definition of 'iteration_head' changed and generalized
 ****************************************************************/
/****************************************************************
 * Changes to be expected in next versions:
 * 1) 'option_statement' is incomplete as it is currently unknown 
 * which kinds of options will be finally required.
 ****************************************************************/
// \subsection{Tokenizer}
// \label{s:tokenizer}
// The tokenizer $\mathcal{T}$ in \ref{e:programminglanguage} is defined through the properties below.
// Let $W \subseteq C$ be the set consisting of the whitespace, tab and new line characters. Let $V \subseteq C$
// be union of $W$ and the elements
//   \begin{equation*}
//   \verb|;| \   \ \verb|:| \   \ \verb|.| \  \ \verb|,| \   \ \verb|+| \   \ \verb|-| \   \ \verb|*| \   \ \verb|/| \   \
//   \verb|^| \   \ \verb|=| \  \ \verb|[| \   \ \verb|]| \   \ \verb|(| \   \ \verb|)| \   \
//   \verb|{| \   \ \verb|}| 
//   \end{equation*}
// Let $x,y,z \in C^\ast$, let $a,b \in V$.
// The properties are:
// \begin{itemize}
// \renewcommand\labelitemi{$\cdot$}
// \item $\mathcal{T}(x(\verb|/|,\verb|*|)z(\verb|*|,\verb|/|)y) = \mathcal{T}(x)\mathcal{T}(y)$ 
// (This formalizes the concept of comments in a programming language);
// \item $\mathcal{T}(x(a)c(b)y)=\mathcal{T}(x)(u)\mathcal{T}(y)$ and $\mathcal{T}(c(b)y)=(u)\mathcal{T}(y)$ for every pair $(c,u)$ as below ($c$ is a keyword in the programming language):
// \begin{itemize}
// \renewcommand\labelitemii{$\cdot$}
// \item $((\verb|R|,\verb|e|,\verb|a|,\verb|l|),\verb|REAL|)$
// \item $((\verb|I|,\verb|n|,\verb|t|,\verb|e|,\verb|r|,\verb|v|,\verb|a|,\verb|l|),\verb|INTERVAL|)$
// \item $((\verb|i|,\verb|n|),\verb|IN|)$
// \item $((\verb|t|,\verb|o|),\verb|TO|)$
// \item $((\verb|f|,\verb|o|,\verb|r|),\verb|FOR|)$
// \item $((\verb|d|,\verb|i|,\verb|f|,\verb|f|),\verb|DIFF|)$
// \item $((\verb|i|,\verb|n|,\verb|i|,\verb|t|,\verb|i|,\verb|a|,\verb|l|,\verb|v|,\verb|a|,\verb|l|,\verb|u|,\verb|e|),\verb|INITIALVALUE|)$
// \item $((\verb|O|,\verb|p|,\verb|t|,\verb|i|,\verb|o|,\verb|n|),\verb|OPTION|)$
// \item $((\verb|o|,\verb|r|,\verb|d|,\verb|e|,\verb|r|),\verb|ORDER|)$
// \item $((\verb|P|,\verb|i|),\verb|_PI|)$
// \item $((\verb|a|,\verb|t|,\verb|a|,\verb|n|),\verb|_ATAN|)$
// \item $((\verb|c|,\verb|o|,\verb|s|),\verb|_COS|)$
// \item $((\verb|c|,\verb|o|,\verb|s|,\verb|h|),\verb|_COSH|)$
// \item $((\verb|e|,\verb|x|,\verb|p|),\verb|_EXP|)$
// \item $((\verb|l|,\verb|n|),\verb|_LN|)$
// \item $((\verb|s|,\verb|i|,\verb|n|),\verb|_SIN|)$
// \item $((\verb|s|,\verb|i|,\verb|n|,\verb|h|),\verb|_SINH|)$
// \item $((\verb|s|,\verb|q|,\verb|r|,\verb|t|),\verb|_SQRT|)$
// \item $((\verb|t|,\verb|a|,\verb|n|),\verb|_TAN|)$
// \end{itemize}
// \item $\mathcal{T}(xcy)= \mathcal{T}(x)\mathcal{T}(y)$, whenever $c \in W^\ast$ and none of the properties above can be applied to $xcy$;
// \item $\mathcal{T}(z) = z$, whenever none of the properties above can be applied to $z$;
// \end{itemize}
// \bibliography{/home/aweber/Data/aweberX230/xDokumente/Notes/Mai2016/texfiles/lit_tmp}
// \end{document}
