#! /usr/bin/env bash

# Make sure you're in the 'Code/latex' directory.
[ "$(basename $PWD)" != 'latex' ] && echo 'Move to the Code/latex directory.' && exit -1

marginal_path="${PWD}/../../Figures/gg3DayPlots/marginal-qa"
high_path="${PWD}/../../Figures/gg3DayPlots/high-qa"
check_path="${PWD}/../../Figures/3DayPlots/high-qa"
length="$(ls -1 $marginal_path | wc -l)"

declare -a marginal high

for file in "$marginal_path"/*; do
    marginal+=("$file")
done
for file in "$high_path"/*; do
    high+=("$file")
done

out='./plots.tex'
[ -f $out ] && rm $out
touch $out
aux_out='./aux'
[ -d $aux_out ] && rm -r $aux_out
mkdir $aux_out
pdf_name='./aux/plots.pdf'

cat input.txt | sed '17,19d' >> $out

for ((i=0; i<length; ++i)); do
    if [ "$i" -lt "99" ] && [ "$(($i%5))" -eq "0" ] && [ "$i" -ne "0" ]; then
        echo -e '\\end{figure}\n\\begin{figure}[H]\n\t\\ContinuedFloat\n\t\\centering' >> $out
    elif [ "$i" -ge "99" ] && [ "$(($i%5))" -eq "4" ]; then
        echo -e '\\end{figure}\n\\begin{figure}[H]\n\t\\ContinuedFloat\n\t\\centering' >> $out
    fi

    high_base="$(basename ${high[$i]})"

    if test -f "${check_path}/${high_base}"; then
        echo -e '\t\\begin{subfigure}{0.48\\linewidth}' >> $out
        echo -en '\t\t\\includegraphics[width=\\linewidth]' >> $out
        echo "{marginal-qa/$(basename ${marginal[$i]})}" >> $out
        echo -e '\t\\end{subfigure}' >> $out

        echo -e '\t\\begin{subfigure}{0.48\\linewidth}' >> $out
        echo -en '\t\t\\includegraphics[width=\\linewidth]' >> $out
        echo "{high-qa/$high_base}" >> $out
        echo -e '\t\\end{subfigure}' >> $out
    else
        echo -e '\t\\begin{subfigure}{\\linewidth}' >> $out
        echo -e '\t\t\\hspace{0.25cm}' >> $out
        echo -en '\t\t\\includegraphics[width=0.48\\linewidth]' >> $out
        echo "{marginal-qa/$(basename ${marginal[$i]})}" >> $out
        echo -e '\t\\end{subfigure}' >> $out
    fi

done

cat input.txt | sed '17,19!d' >> $out

pdflatex -output-directory $aux_out $out > /dev/null 2>&1
mv $pdf_name .
