%%
%% This is file `rapport.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% rapport.dtx  (with options: `class')
%% -------:| ---------------------------------------------------------------------
%% rapport:| This package extends the article document class for students reports.
%%  Author:| Marie Pelleau
%%  E-mail:| marie.pelleau@univ-cotedazur.fr
%% License:| Released under the LaTeX Project Public License v1.3c or later
%%     See:| http://www.latex-project.org/lppl.txt
%% 
\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesClass{rapport}
    [2021/11/22 v1.10 adding the titlepageneglish macro.]
\LoadClass[a4paper, twoside]{article}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax


\RequirePackage{geometry,tikz,afterpage}


\RequirePackage{titling,fancyhdr}
\RequirePackage{graphicx}
\RequirePackage{enumitem}
\RequirePackage{amsthm}
\RequirePackage{amssymb}
\RequirePackage{xspace}
\usepackage{multicol}
% To read csv files
% \usepackage{filecontents}
% To plot graphs
\usepackage{pgfplots}
\pgfplotsset{compat=1.7}


\usepackage[utf8]{inputenc}
% \usepackage[T1]{fontenc}

\usepackage{pifont}
\usepackage{url} 
\usepackage{xstring}
\usepackage{etoolbox}
% \usepackage{syntax}
\usepackage{bm}

\usepackage[colorlinks, citecolor=red!60!green, linkcolor=blue!60!green, urlcolor=magenta]{hyperref}

% \usepackage{algorithm}

\usepackage{float}
% \usepackage{algo}
\usepackage{mathtools}
\usepackage{subcaption}

\usepackage{caption}
\captionsetup[table]{position=below}

\usepackage[capitalise,noabbrev,nameinlink]{cleveref}

\usepackage{tikz}
\usetikzlibrary{automata, positioning, arrows, calc, datavisualization.formats.functions}


\usepackage[linesnumbered,ruled]{algorithm2e}
\usepackage[outputdir=.aux]{minted}

\crefname{algocf}{Algorithm}{Algorithms}

% Bibliography
\usepackage[
backend=biber,
sorting=ynt
]{biblatex}
\addbibresource{bib.bib}

\def\whitepage{
    \newpage
    \thispagestyle{empty}
    \mbox{}
    \newpage
}

\pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markboth{\thesection.\ #1}{}}
\fancyfoot{}

\usepackage[bottom]{footmisc}

\fancyhead[LE]{\textsl{\leftmark}}
\fancyhead[RE, LO]{\textbf{\thepage}}
\fancyhead[RO]{\textsl{\rightmark}}

\def\etc{\textit{etc.}}

\newcommand*{\length}[1]{%
    \@tempcnta\z@
    \@for\@tempa:=#1\do{\advance\@tempcnta\@ne}%
    \the\@tempcnta%
}

\pgfmathdeclarefunction{countarray}{1}{\edef\pgfmathresult{\Counter#1\Stopper}}

\usepackage{xargs}     
\usepackage[textsize=tiny]{todonotes}
\setlength{\marginparwidth}{2cm}
\newcommandx{\unsure}[2][1=]{\todo[linecolor=red,backgroundcolor=red!25,bordercolor=red,#1]{#2}}
\newcommandx{\change}[2][1=]{\todo[linecolor=blue,backgroundcolor=blue!25,bordercolor=blue,#1]{#2}}

\theoremstyle{definition}
\newtheorem{example}{Example}
\newtheorem{theorem}{Theorem}
\newtheorem{definition}{Definition}[section]
\newtheorem{notation}{Notation}
\newtheorem{lemma}{Lemma}

\theoremstyle{remark}
\newtheorem*{remark}{Remark}

\graphicspath{img/}

\makeatletter         
\renewcommand\maketitle{
\newgeometry{top=0.8in,bottom=1in,right=1in,left=1in}


\author{Fissore Davide}
\title{Mini-solver in OCaml based on binary Constraint Satisfaction Problem}

\makeatletter
\let\thetitle\@title
\let\theauthor\@author
\makeatother

\newcommand{\professor}{Jean-Charles Régin}
\newcommand{\course}{Master in Computer Science}
\newcommand{\subcourse}{Course : Constraint Programming}

\newcommand{\HRule}{\rule{\linewidth}{0.5mm}} % Defines a new command for the horizontal lines, change thickness here

\SetKw{KwDownTo}{downto}

\begin{center} % Center everything on the page
 
%--------------------
%	LOGO SECTION
%--------------------

\includegraphics[width=13cm]{img/logos/logo_haut_uca.png}\\[3cm] % Include a department/university logo - this will require the graphicx package
 
 
%--------------------
%	HEADING SECTIONS
%--------------------

% \textsc{\LARGE University Name}\\[1.5cm] % Name of your university/college
\ifdefempty{\subcourse}{}{\textsc{\Large \course}\\[0.5cm]} % Major heading such as course name
\ifdefempty{\subcourse}{}{\textsc{\large \subcourse}\\[0.5cm]} % Minor heading such as course title

%--------------------
%	TITLE SECTION
%--------------------

\HRule \\[0.8cm]
{\huge \bfseries \thetitle}\\[0.4cm] % Title of your document
\HRule \\[1.5cm]
 
%--------------------
%	AUTHOR SECTION
%--------------------

\begin{minipage}{0.4\textwidth}
\begin{flushleft} \large
\emph{Author:}\\
\StrSubstitute{\theauthor}{,}{\newline}% Your name
\end{flushleft}
\end{minipage}
~
\ifdefempty{\professor}{}{
    \begin{minipage}{0.4\textwidth}
    \begin{flushright} \large
    \emph{Supervisor:} \\
    \StrSubstitute{\professor}{,}{\newline}% Supervisor
    \end{flushright}
    \end{minipage}
}


% \begin{abstract}
%     Goal: code a constraint solver using the arc-concistency algorithms: AC-3, AC-4, AC-6, AC-2001
% \end{abstract}

% If you don't want a supervisor, uncomment the two lines below and remove the section above
%\Large \emph{Author:}\\
%John \textsc{Smith}\\[3cm] % Your name

%--------------------
%	DATE SECTION
%--------------------
\mbox{}
\vfill
{\large Februray 2023}

%--------------------
\end{center}
\pagenumbering{gobble}
\whitepage{}
\clearpage

\tableofcontents

\clearpage
\pagenumbering{arabic}
% To start the page counter after the TOC
% \setcounter{page}{1}
}
\makeatother

\newcommand{\neworrenewcommand}[1]{\providecommand{#1}{}\renewcommand{#1}}
