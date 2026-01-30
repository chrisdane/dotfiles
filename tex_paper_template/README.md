# tex template

- reference across documents with:
```
@paper.tex

% reference across files
\usepackage{xr}
\externaldocument[supp:]{supp}

Fig. \ref{supp:fig:supp:a} is in another document, the supplement!
```
and
```
@supp.tex

% reference across files
\usepackage{xr}
\externaldocument{paper}

Fig. \ref{fig:supp:a} is here, in the supp document.

\begin{figure}[H]
\centering\noindent\includegraphics[width=0.25\textwidth]{example-image-a}
\caption{Example A-figure.}
\label{fig:supp:a}
\end{figure}
```

- compile with:
```
pdflatex supp
bibtex supp
pdflatex supp
pdflatex supp
pdflatex paper
bibtex paper
pdflatex paper
pdflatex paper
```

# Change font

Put this in the preamble:
```
\usepackage{ifxetex,ifluatex}
\newif\ifxetexorluatex
\ifxetex
  \xetexorluatextrue
\else
  \ifluatex
    \xetexorluatextrue
  \else
    \xetexorluatexfalse
  \fi
\fi
\ifxetexorluatex
  \usepackage{fontspec}
  %\setmainfont{Linux Libertine O}
  \setmainfont
    [Path = /usr/share/texmf-dist/fonts/opentype/public/newcomputermodern/,
     Extension = .otf,
     UprightFont = *-Regular,
     BoldFont = *-Bold,
     ItalicFont = *-Italic,
     BoldItalicFont = *-BoldItalic
    ]{NewCM10}
\else % not compiled with lualatex: fontspec not available
  %\usepackage[T1]{fontenc}
  %\usepackage[nomath]{lmodern}
  %\usepackage[scaled=.97]{helvet}
  %\usepackage[]{libertine}
\fi % if \ifxetexorluatex
%\renewcommand\familydefault{\sfdefault}
```

First, check if document is compiled with xelatex or lualatex (--> fonspec package available) or not
Then check the compiled pdf with `pdffonts compiled.pdf`

