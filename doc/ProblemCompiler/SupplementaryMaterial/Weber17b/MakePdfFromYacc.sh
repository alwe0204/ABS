#!/bin/sh
######################################################################
### $Id: MakePdfFromYacc.sh 2072 2020-06-11 12:10:32Z lf1agure $
### Alexander Weber, 2017
###
### Script to produce Alexander Weber's language definition [Weber17b]
### Final output produced: Weber17b.pdf
###
### BibTeX keys coincide with Gunther Reissig's BibTeX database, but
### list of references is currently hard coded in the file
### Weber17b.bbl
######################################################################
sed ':again;$!N;$!b again; s/\s{[^}]*}//g' parse.y > doctmp.tex; #delete everything inside curly brackets.
sed -i '/^%union/ d' doctmp.tex ;
sed -i '/^%type/ d' doctmp.tex ;
sed -i '/^%{/ d' doctmp.tex ;
sed -i '/^%}/ d' doctmp.tex ;
sed -i '/^#/ d' doctmp.tex ;
sed -i '/^extern/ d' doctmp.tex ;
sed -i '/^static/ d' doctmp.tex ;
sed -i '/^void/ d' doctmp.tex ;
sed -i '/^%left/ d' doctmp.tex ;
sed -i '/^%right/ d' doctmp.tex ;
sed -i '/^%token/ d' doctmp.tex ;
sed -i '/^%start/ d' doctmp.tex ;
sed -i '/^%%/ d' doctmp.tex ;
sed -i 's/%empty//g' doctmp.tex;
sed -i 's/\/\*/\n%%/g' doctmp.tex;
sed -i '/^%%/ d' doctmp.tex ;
awk '/^\"/ && $3== "return" {print $4 > "keywords.dat" } ' < lex.l  #extract the keywords in the lex file
awk '/^\"/ && $3== "return" {print "// \\item $\\mathcal{T}(xa\\verb|" $1 "|by)=\\mathcal{T}(x)(\\verb|"$4"|)\\mathcal{T}(y)$;" > "keywords.tex" } ' < lex.l  #extract the keywords in the lex file
awk '/^\"/ && $3== "return" {print $1 " " $4 > "keywords2mma.dat" } ' < lex.l  #extract the keywords in the lex file
while read p; do
var=$p
sed -i '/\/\//b; s/[[:space:]]'"$var"'[[:space:]]/'"'"''$var''"'"' /g' doctmp.tex ; #the convention is that terminals are written within quotes in the documentation.
done < keywords.dat
sed -i 's/\/\// /g' doctmp.tex ;
sed -i '/^\s\*/ d' doctmp.tex ;
sed -i '/^\*\// d' doctmp.tex ;
sed -i '/^\*\// d' doctmp.tex ;
sed -i '/^\s*$/d' doctmp.tex ; #delete empty lines
# BEGIN: compose file LanguageDefinition.tex
# Here, the section following the Introduction is contained in the
# temporary file doctmp.tex, which has been produced from the file
# parse.y
/bin/cat Introduction.tex > LanguageDefinition.tex ;
head -n -2 doctmp.tex >> LanguageDefinition.tex ; # without trailing lines: \bibliography{...}\end{document}
/bin/cat Compiler.tex >> LanguageDefinition.tex ;
# list of references is hard coded in the file LanguageDefinition.bbl:
/bin/echo "\bibliography{}\end{document}" >> LanguageDefinition.tex ;
# END: compose file LanguageDefinition.tex
#
# remove temporary files:
/bin/rm doctmp.tex keywords2mma.dat keywords.dat keywords.tex;
#
# compile file LanguageDefinition.tex:
/bin/cp Weber17b.bbl LanguageDefinition.bbl
pdflatex LanguageDefinition.tex;
pdflatex LanguageDefinition.tex;
pdflatex LanguageDefinition.tex;
#
# remove temporary files:
/bin/rm LanguageDefinition.tex LanguageDefinition.bbl LanguageDefinition.out LanguageDefinition.log LanguageDefinition.aux LanguageDefinition.toc
# move LanguageDefinition.pdf to final destination Weber17b.pdf:
/bin/mv LanguageDefinition.pdf Weber17b.pdf
