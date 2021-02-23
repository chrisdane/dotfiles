# notes to this minimalistic latex paper template

This minimalistic Latex paper template supports
- proper page and figure/table numbering
- distinguishes between figure/table numbering in the main versus appendix parts
- properly lists all sections as TOC entries in pdf viewer also if a section has no number
- supports longtables
- tries to be as minimal as possible

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

