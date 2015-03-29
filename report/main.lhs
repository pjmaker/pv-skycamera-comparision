% 
% main.lhs - the body for the skycamera comparision
%
% See the Notes to Authors section for how it is laid out.
%

\excludecomment{figcode:bit} % note that including this sometimes break things
\excludecomment{testpattern:bit}
\excludecomment{imports:bit}

%% title - the title page
\title{Comparing Sky Camera Solar Forecasting Systems for PV/Diesel Systems}
\author{
Colin Bonner, Antonin Braun, Thomas Loveard, %
Phil Maker and Marc Mueller-Stoffels.
\thanks{Colin Bonner is with %
\href{http://www.fulcrum3d.com}{Fulcrum 3D}}
\thanks{Antonin Braun is with %
\href{http://www.reuniwatt.com}{Reuniwatt}}
\thanks{Thomas Loveard is with %
\href{http://www.pixelscience.com.au}{Pixel Sciences}}
\thanks{Phil Maker is with %
\href{http://www.powerwater.com.au/}{Powerwater} working on the 
\href{http://www.powerwater.com.au/}{SETuP} project 
and also helps the
\href{http://www.uaf.edu/acep}{Alaskan Center for Energy and Power}
occasionally. He has worked on a variety of embedded systems (inside peopel) and hybrid power systems including the RIWE
and Coral Bay}.
\thanks{Marc Mueller-Stoffels is with 
\href{http://www.uaf.edu/acep}{Alaskan Center for Energy and Power}}
\thanks{Manuscript received April 19, 20015; revised December 27, 2012
via time travel.}}
\markboth{Journal of Zero Point Energy, Volume~2, No~10}{How
to extract ZPE (Draft only) )}
\IEEEpubid{0000--0000/00\$00.00 ̃\copyright ̃2007 IEEE}
\maketitle
\begin{abstract}
Always write abstract at the end, or is it the beginning. 
Regardless this is a trial.
\end{abstract}
\begin{IEEEkeywords} 
\LaTeX, Haskell, Hybrid Power System. Solar nowcasting, Sky Camera,
PV/Diesel.
\end{IEEEkeywords} 

%% contents, etc
\tableofcontents
\listoffigures
\listoftables
\newpage

%% notestoauthors
\section{Notes to Authors}

This section is intended to summarise our cunning plan as
authors, technology providers, grant agencies and utilities.
It will not appear in the final report.

Please do not take the first draft too seriously.

\subsection{How are we going to do it?}
This paper/comparision is based on previous work comparing 
different control systems, in particular standard PV/Diesel
control vs perfect prediction. 

The method is to use Literate Haskell as a tool to buildup a framework
for the comparision.  We could of course use other methods (MATLAB)
but its kind of nice to have something that is:

\begin{itemize}
\item Free/open source including the model, its engine and tools.
\item Complete, at least in the full version of this paper including 
  all the annexes.
\item Short since its in a functional programming language (see
  Haskell \cite{Haskell}).
\item At least reasonably well tested using property based testing,
  see QuickCheck \cite{QuickCheck}.
\end{itemize}

\subsection{Administration}
\begin{enumerate}
\item The paper contents will be kept on github in a possibly private 
  archive with access by all authors. All nominated authors
  have veto over the contents, if not the results. In addition 
  we have a few other people providing input/review,.
\item They are formatted in \LaTeX~using an IEEE style for now.
\item In terms of editing using a text editor on +main.lhs+ should get
you a long way. If +git+ is a bit painful send me the text or changes 
and I will merge them. Give me a ring for details.
\item More to follow.
\end{enumerate}

\subsection{Proposed Roles and Responsibilities}

In terms of how this will work:

\begin{enumerate}
\item My guess is I'll do most of the editing/coding with
  approval from everyone else.
\item Perhaps a few sections can be shared amongst the other 
  authors, my initial thoughts are:
  \begin{enumerate}
  \item Colin/Antonin/Thomas: a review of the testing methodology itself.
  \item Marc: I'd like to bring in once we have a draft for a 
    fresh pair of eyes (both physics/CS and academic). Marc has a
    few ideas and has already talked to Thomas.
  \item Phil: I'm just here to help but expect to do the coding
    and definitions for the comparision methodology with your kind
    assistance. 
  \end{enumerate}
\end{enumerate}

\subsection{General Questions to the Authors}
And finally some questions to the gentle authors:

\begin{enumerate}
\item What are the critical assumptions?
\item What do we want in the results?
\item How should we compare solutions?
\item When should the first phase of the trial finish?
\end{enumerate}
\newpage

%% introduction - tell them what we are going to do
\section{Introduction}

The use of sky camera solar forecasting within PV/Diesel systems
promises significant improvements in system performance. The
SETuP \footnote{SETuP is described as ...} project has funded a trial
of this technology in the Northern Territory.

Usual fluff and buff.

Explain what goes where.

\begin{imports:bit}
\section{Haskell Header and Imports}
\begin{code}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import System.IO
import System.Random
import System.Cmd
import System.Environment
import Text.Printf
import Data.Packed.Vector 
import Data.List
import Numeric.GSL.Statistics
import Graphics.Rendering.Plot
import Control.Exception
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck 
import Test.QuickCheck.All
import Debug.Trace
import Text.Regex.Posix
import System.CPUTime
\end{code}
\end{imports:bit}

\subsection{Characteristics of PV Systems} 
PV output is rapidly effected by cloud events, a Powerwater
rule of thumb for a 300kW sized array is that a cloud event will
reduce output from 100\% to 20\% within 6..10s occur and must be
covered by station spinning reserve. 

The justification for the 6s is that:

\begin{enumerate}
\item Solar output varies with wind speed, at altitude the speed
  might be 10m/s.
\item A centralised solar array of 300kW size might be 60m across.
\end{enumerate}

Since typical diesel generater start and synchronise times are in the
20..90s range we then need to carry spare capacity (Spinning Reserve)
capable of dealing with cloud events or use energy storage or
demand management.

Do we want to deal with Schrodingers Law and stability here as well.
(uhm perhaps not).

A typical weeks worth of +PvAvailP+ data shows significant solar
variation over the time scales discussed.It is also worth noting that
4 out of the 7 days are very stable ad whence suitable for running a
smaller diesel.

\begin{figcode:bit}
\begin{code}
nhrs = 6 * 30 * 24
tdefault = tselect (nhrs * h) (32 * w + 68 * h)
sPvAvailP = tdefault $ scale 2 $ read1s "../data/KalkPvAvailP.1s"
-- sPvP = tdefault $ read "PvP.1s" -- real data

figPvAvailP1 = fig "figPvAvailP1.pdf" 
                 [figLineStats (sPvAvailP,"PvAvailP",green)]
                 figCfg{plottitle="PV Available"}
\end{code}
\end{figcode:bit}

\begin{figure}[H]
  \includegraphics[width=1.0\columnwidth]{figPvAvailP1.pdf}
  \caption{PV Available}\label{figPvAvailP1}
\end{figure}

Given a diesel start, synchronisation and ramp up time for 
a diesel of 60s it is considered that a 120s (2 minute) prediction
that PV is going to drop would be most useful. 
Figure \ref{figPvAvailMaxDown120P1} shows the maximum downwards 
variation in PV output over a 120s window.

\begin{code}
sPvAvailMaxDown120P = maxdown 120 sPvAvailP
sPvAvailMaxDown120PAboveSpin = map aboveSpin sPvAvailMaxDown120P 
  where aboveSpin x = if x < 50 then 0 else x
\end{code}
\begin{figcode:bit}
\begin{code}
figPvAvailMaxDown120P1 = fig "figPvAvailMaxDown120P1.pdf" 
               [figLineStats (sPvAvailMaxDown120P,"PvAvailMaxDown120P",green)]
               figCfg{plottitle="PV Max Down 120s"}
\end{code}
\end{figcode:bit}
\begin{figure}[H]
  \includegraphics[width=1.0\columnwidth]{figPvAvailMaxDown120P1.pdf}
  \caption{PV Available Max Down over 120s}\label{figPvAvailMaxDown120P1}
\end{figure}

Figure \ref{figPvAvailMaxDown120PAboveSpin1} show those drops which
are above the system spinning reserve of 50kW (+SpinPPa+).

\begin{figcode:bit}
\begin{code}
figPvAvailMaxDown120PAboveSpin1 = fig "figPvAvailMaxDown120PAboveSpin1.pdf" 
               [figLineStats (sPvAvailMaxDown120PAboveSpin,
                 "PvAvailMaxDown120PAboveSpin",red)]
               figCfg{plottitle="PV Max Down 120s Above Spin"}
\end{code}
\end{figcode:bit}
\begin{figure}[H]
  \includegraphics[width=1.0\columnwidth]{figPvAvailMaxDown120PAboveSpin1.pdf}
  \caption{PV Available Max Down over 120s above Spinning Reserve}
  \label{figPvAvailMaxDown120PAboveSpin1}
\end{figure}

\subsection{Characteristics of Diesel Systems}

Describe how they work, synchronisation, etc.

\subsection{Characteristics of Diesel Generator Fuel Consumption}
In this section we will describe the diesel fuel consumption model
that will be used in this paper. This particular model whilst well
known (see  ...) is perhaps underused by some practitioners since diesel
generator fuel consumption is usually presented as a non linear
function as in fig \ref{figFuelCurve1}. The summary of the model
and its uses are:
 
\begin{enumerate}
\item Fuel consumption variation is nearly linear and varies with both load
  and capacity online.
\item Fuel consumption can be modelled as two independent components:
  \begin{enumerate}
  \item \texttt{capCost} - the cost of maintaining capacity online
    regardless of load. This cost is independent of the current load
    and can be thought of as the standby losses for running the
    diesels at a fixed speed in a synchronous generater (e.g. 1500rpm).
  \item \texttt{energyCost} - the cost of energy production in L/kWh
    which is approximately the same across diesels sizes in the range
    100kW to 1MW. There is an improvement as engine size scales but it 
    is not considered significant across the range discussed.
  \end{enumerate}
\item This allows us to place bounds on possible performance
  improvements without doing any detailed modelling. For example:
  \begin{enumerate}
  \item In a diesel only system any change to generator configuration
    can only effect the \texttt{capCost} component, not the
    \texttt{energyCost}.
  \item So for a system with N $\times$ 320kW generators a control system
    improvement that turns off a single generator 30 minutes earlier
    each day can only result in a saving of \texttt{capCost 320} which
    is around 12\litre\per\hour> so that total saving is $12\times 0.5 \times 365
    = 2190$ L/year.  
  \end{enumerate}
\item We can also provide estimates of the cost of keeping spinning
  reserve online in a simple manner based on \texttt{capCost} and
  typical system loadings.
\end{enumerate}

Traditionally fuel consumption is presented using a kWh efficiency
curve such as fig \ref{figFuelCurve1}. This is clearly
non-linear and shows that at low loads fuel consumption per kWh is
typically twice that at full load.

\begin{figure}[H] 
  \includegraphics[width=1.0\columnwidth]{figFuelCurve1.pdf}
  \caption{Fuel Consumption in L/kWh vs Load is non linear}
  \label{figFuelCurve1}
\end{figure}

However the same data presented with L/h rather than L/kWh results in
fig \ref{figFuelCurve2} which shows clearly that fuel
consumption is close to linear for a given engine and that the losses
at 0\% load result in most of the efficiency decrease at low load in fig
\ref{figFuelCurve1}.

\begin{figure}[H] 
  \includegraphics[width=1.0\columnwidth]{figFuelCurve2.pdf}
  \caption{Fuel Consumption vs Load is very close linear}
  \label{figFuelCurve2}
\end{figure}

From fig \ref{figFuelCurve2} it
is apparent that the 
fuel consumption rate can be modelled using two constants
\texttt{capCost} and \texttt{energyCost}:

\begin{enumerate}
\item \texttt{capCost}: is the fuel in L/h required at 0\% load in order to
  run the engine at its synchronous speed. .
\item \texttt{energyCost}: the cost in L/kWh for producing a single
  kWh. Modern engines are fairly close in efficency terms at the same
  load factor.
\end{enumerate}

The \texttt{capCost} for a generator is typically around 12\% of its
fuel consumption at full load. This numbers scales with generator
size as is to be expected though a further study to develop detailed
scaling laws for these systems would be most worthwhile.

It should also be noted that in most systems generaters do not spend 
a significant time at 100\% load because of spinning reserve requirements
so the non linearity at high loads is reduced.

\subsubsection{Total fuel used calculation}

The total fuel used will be calculated by 
summing from the online \texttt{capacitys} and \texttt{loads}. 
The particular constants are based on the 328kW Caterpillar Generator.

\begin{code}
-- Buglet if loads > capacitys no ngen generation ttt
fuelUsedTotal :: [Double] -> [Double] -> Double 
fuelUsedTotal capacitys loads = (capCost * sum capacitys) + 
                                (energyCost * sum loads)

fuelUsedAt :: Double -> Double -> Double
fuelUsedAt capacity load = (capCost * capacity) + (energyCost * load)

sfuelUsed :: [Double] -> [Double] -> [Double]
sfuelUsed [] [] = []
sfuelUsed (c:cs) (l:ls) = (fuelUsedAt c l):sfuelUsed cs ls
sfuelUsed cs ls = error $ "fatal: sfuelUsed has leftover values " ++ 
                  "cs " ++ show cs ++ " ls " ++ show ls


capCost = (fuelUsed0Pct/fuelUsedRating)/3600.0
energyCost = ((fuelUsed100Pct-fuelUsed0Pct)/fuelUsedRating)/3600.0

fuelUsedRating = 328.0 -- kW rating for diesel
fuelUsed0Pct = 11.0    -- L/h at 0% load
fuelUsed100Pct = 83.0  -- L/h at 100% load
\end{code}

\subsubsection{Fuel Usages Examples}
The following examples may be helpful

\begin{comment}
\begin{code}
examples_fuelUsed = do 
  examples "examples_fuelUsed.txt" display exprn isok tests 
  where display ((a,b),ok) = "3600.0 * fuelUsed" ++ " " ++ 
                             (show a) ++ " " ++ (show b)        
        exprn ((a,b), ok) = 3600.0 * fuelUsedTotal a b
        isok ((a,b), ok) = exprn((a,b),ok) == ok
        tests = [(([0],[0]),0), 
                 (([328],[0]),11),
                 (([328],[328]), 83.0)]
\end{code}
\end{comment}
\VerbatimInput{examples_fuelUsed.txt}

\subsubsection{Diesel Generator Fuel Consumption Source Data}

The source data is in presented in the following 
table along with the values for a simple Estimate based
on the described method.

\begin{table}[H]
\begin{center}
\begin{tabular}{|l|l|l|l|l|}
\hline
Real	& Real	& Real	& Real	        & Estimated \\
kWe	&\%Load& L/h	& L/kWh	&         L/h \\
\hline
328	& 100	& 86	& 0.262         & 83 \\
295.2	& 90	& 77.1	& 0.261  	& 75.8 \\
262.4	& 80	& 68.4	& 0.260		& 68.6 \\
229.6	& 70	& 61	& 0.265		& 61.4 \\
196.8	& 60	& 53.8	& 0.273		& 54.2 \\
164	& 50	& 46.6	& 0.284		& 47 \\
131.2	& 40	& 39.5	& 0.301		& 39.8 \\
98.4	& 30	& 32.4	& 0.329		& 32.6 \\
65.5	& 20	& 25.3	& 0.386		& 25.3 \\
32.8	& 10	& 18	& 0.548		& 18.2 \\
0.0     & 0     & 12    & undefined     & 11 \\
\hline
\end{tabular}
\caption{Caterpillar 328kW Generator Data Sheet}
\label{tableDataSheet}
\end{center}
\end{table}

\subsubsection{Fuel usage as generator size and kW load increases}

Using the proposed model for fuel consumption we end up with the following 
approximations for fuel consumption and energy cost vs Load and Generator size
assuming a fleet of units of the same size.

\begin{figure}
  \includegraphics[width=1.0\columnwidth]{figFuelCurve3.pdf}
  \caption{Fuel Consumption vs Load and Generator Size}\label{figFuelCurve3}
\end{figure}

The same data presented in L/kWh results in fig \ref{figFuelCurve4}.

\begin{figure}
\includegraphics[width=1.0\columnwidth]{figFuelCurve4.pdf}
\caption{Energy Cost vs Load and Generator Size}\label{figFuelCurve4}
\end{figure}

The final plots shows the cost in L/y versus Generator Capacity and load.

\begin{figure}
\includegraphics[width=1.0\columnwidth]{figFuelCurve5.pdf}
\caption{Total Cost per year vs Load and Generator Size}\label{figFuelCurve5}
\end{figure}
 

 % figFuelCurve1,2 generation
\begin{figcode:bit}
\begin{code}
-- these are all derived from the Caterpillar data sheet and the above list
fuelCurveActualLperkWh = -- L/kWh from 10%..100% (0% is undefined!)
  [10000,0.548,0.386,0.329,0.301,0.284,0.273,0.265,0.260,0.261,0.262]
fuelCurveActual = 
  [12,18,25.3,32.4,39.5,46.6,53.8,61,68.4,77.1,86]
fuelCurveEstimate =  
  [11,18.2,25.3,32.6,39.8,47,54.2,61.4,68.6,75.8,83]
\end{code}
\end{figcode:bit}
%% 
\begin{figcode:bit}
\begin{code}
figFuelCurve1 = do  
  fig "figFuelCurve1.pdf" 
    [figPoint (fuelCurveActualLperkWh, "Actual L/kWh", brown)]
    figCfg{plottitle="Caterpillar 320kW Efficiency",
           xgrid=True,ygrid=True,
           xtitle="Load %",xlow=0, xstep=10, xhigh=100,xticks=11,
           ytitle="L/kWh", ylow=0, yhigh=0.7, yfmt="%.2f",yticks=11}
\end{code}
\end{figcode:bit}
%% 
\begin{figcode:bit}
\begin{code}
figFuelCurve2 = do 
   fig "figFuelCurve2.pdf" 
        [figLine (fuelCurveEstimate, "Estimate", red),
        figPoint (fuelCurveActual, "Actual", brown)]
        figCfg{
          plottitle="Caterpillar 320kW Fuel Consumption",
          xgrid=True,ygrid=True,
          xtitle="Load %",xlow=0, xstep=10, xhigh=100,xticks=11,
          ytitle="L/h",ylow=0, yhigh=100, yfmt="%.1f",yticks=11}
\end{code}
\end{figcode:bit}

%figFuelCurve3
\begin{figcode:bit}
\begin{code}
figFuelCurve3y :: [Double]
figFuelCurve3y = [0..1000] 
figFuelUsed2 :: Double -> Double -> Double
figFuelUsed2 cap load = 3600.0 * fuelUsedTotal [ncap] [load]
  where ngen = fromInteger (ceiling (load/cap)) :: Double
        ncap = ngen * cap
figFuelCurve3g cap = map (figFuelUsed2 cap) figFuelCurve3y
figFuelCurve3 = do
  fig "figFuelCurve3.pdf" 
    [figLine (figFuelCurve3g 100.0, "100kW", brown),
     figLine (figFuelCurve3g 300.0, "300kW", green),
     figLine (figFuelCurve3g 500.0, "500kW", blue)
    ]
    figCfg{
      plottitle="Fuel Consumption for varying generator size",
      xtitle="Load kW",xlow=0,xstep=1,xhigh=1000,xticks=11,
      xfmt="%.0f",
      ytitle="L/h",ylow=0,yhigh=300,yfmt="%.0f"}
\end{code}
\end{figcode:bit} 

%figFuelCurve4
\begin{figcode:bit}
\begin{code}
figFuelCurve4y :: [Double]
figFuelCurve4y = [0..1000] 
figFuelUsed4 :: Double -> Double -> Double
figFuelUsed4 cap load = 3600.0 * ((fuelUsedTotal [ncap] [load])/load)
  where ngen = fromInteger (ceiling (load/cap)) :: Double
        ncap = ngen * cap
figFuelCurve4g cap = map (figFuelUsed4 cap) figFuelCurve4y

figFuelCurve4 = 
  fig "figFuelCurve4.pdf" 
    [figLine (figFuelCurve4g 100.0, "100kW", red),
     figLine (figFuelCurve4g 300.0, "300kW", green),
     figLine (figFuelCurve4g 500.0, "500kW", blue)]
    figCfg{
      plottitle="Fuel Efficiency vs Load with varying Generator Size",
      xtitle="Load kW",xlow=0,xstep=1,xhigh=1000,xticks=11,
      xfmt="%.0f",
      ytitle="L/kWh",ylow=0,yhigh=1,yfmt="%.2f"}
\end{code}
\end{figcode:bit}

%figFuelCurve5
\begin{figcode:bit}
\begin{code}
figFuelCurve5y :: [Double]
figFuelCurve5y = [0..1000] 
figFuelUsed5 :: Double -> Double -> Double
figFuelUsed5 cap load = 24 * 365 * 3600.0 * (fuelUsedAt ncap load)
  where ngen = fromInteger (ceiling (load/cap)) :: Double
        ncap = ngen * cap
figFuelCurve5g cap = map (figFuelUsed5 cap) figFuelCurve5y

figFuelCurve5 = do
  fig "figFuelCurve5.pdf"
    [figLine (figFuelCurve5g 100.0, "100kW", red),
     figLine (figFuelCurve5g 300.0, "300kW", green),
     figLine (figFuelCurve5g 500.0, "500kW", blue)
    ]  
    figCfg{xtitle="Generator Size kW",xlow=0,xstep=1,xhigh=1000,xticks=10,
            xfmt="%.0f",
            ytitle="L/y",ylow=0,yhigh=3e6,yticks=5,
            yfmt="%.2f"}
\end{code}
\end{figcode:bit}

\subsection{On Stability and distributed PV}

Do we need to talk about stability \cite{Schrodinger44},

I think so..
\subsection{Benefits}

With 3 x 320kW Detroit Series 60 Diesels:

During 5 out of the 7 days we could run 1 rather
than 2 for a typical load.If the minimum loading is ...
then ...

\begin{code}
-- A
\end{code}

%% method - tell them how we are to do it
\section{Method}
How are we going to do it.
\subsection{Trial Outline Plan}

\subsection{Trial Location and Timing}

\subsection{Completion of the Phase 1 Trial}

The Phase 1 trail will be completed when either:

\begin{itemize}
\item 6 months has elapsed.
\item At least 100 significant events which demonstrate:
  \begin{itemize}
  \item Drop in +PvAvailP+ by at least 50\% over 2 minutes.
  \item The Drop occurs from a cloud free value of at least 
    60\% of rated.
  \item Events are separated by at least 30 minutes.
  \end{itemize}
\end{itemize}

Preliminary results will be released at 3 months.

\begin{code}
sKalkPvAvailP = take 10000000 $ read1s "../data/KalkPvAvailP.1s"

events xs = if (length xs) > 2*6000 
  then if d > 1 then d:events (drop 12000 xs) 
       else events (drop 12000 xs)
  else []
  where d = before - after 
        before = (minimum $ take 6000 xs) 
        after = (minimum $ take 6000 $ take 6000 xs)
\end{code}

\begin{code}
figKalkPvAvailP = fig "figKalkPvAvailP.pdf"
  [figLine (sKalkPvAvailP, "KalkPvAvailP", red)]
  figCfg
\end{code}
%%\subsection{Method 0}

\begin{verbatim}
= (ramp 1 0.1 sPvAvailP)
\end{verbatim}

\subsection{Method 1}

\begin{verbatim}
A
\end{verbatim}

\subsection{Method 2}

\begin{verbatim}
A
\end{verbatim}

%% results - tell them what the results are
\section{Results}
What are the results.

In a series of tables etc.

\subsection{Results A}

\subsection{Results B}

%% conclusion/final bit
\section{Conclusion}
So what.

%% acknowledgement - who to than

\section{Acknowledgments}
Thanks to my mother.

%% bibiography
\bibliographystyle{IEEEtran}
\bibliography{main}

%% authors
\begin{IEEEbiography}[{\includegraphics[width=1in,
height=1.25in,clip,keepaspectratio]{logo.jpg}}]{Colin Bonner}
is a director with Fulcrum 3D which ....
\end{IEEEbiography}
\begin{IEEEbiography}[{\includegraphics[width=1in,
height=1.25in,clip,keepaspectratio]{logo.jpg}}]{Antonin Braun}
is with Reuniwatt solving the worlds problems.
\end{IEEEbiography}
\begin{IEEEbiography}[{\includegraphics[width=1in,
height=1.25in,clip,keepaspectratio]{logo.jpg}}]{Thomas Loveard}
is very good looking
\end{IEEEbiography}
\begin{IEEEbiography}[{\includegraphics[width=1in,
height=1.25in,clip,keepaspectratio]{logo.jpg}}]{Phil Maker}
Phil Maker is working at Powerwater on the SETuP project as
a controls engineer. His other role is Research Adjunct Professor 
at the Alaskan Center for Energy and Power (ACEP, acep.uaf.edu). 
Previous work includes the development of a distributed 
control system for high penetration power systems and
work on an early embedded (inside people) system (4210 Defibrillator).
\end{IEEEbiography}

\begin{IEEEbiography}[{\includegraphics[width=1in,
height=1.25in,clip,keepaspectratio]{marc-shot.jpg}}]{Dr. Marc Mueller-Stoffels} 
is the Director for the Power Systems Integration Program at the
Alaska Center for Energy and Power (ACEP, acep.uaf.edu) and Research
Assistant Professor at the University of Alaska Fairbanks' Institute
of Northern Engineering. Marc's research focuses on the integration of
variable generation sources into isolated microgrids. Most recently,
he has lead the testing of an inverter-battery system to enable
diesel-off mode, and a flywheel energy storage system for power
quality applications in high contribution renewable energy
scenarios. Prior to joining ACEP, Marc has developed regional scale
climate models with focus on Arctic sea ice, and has chaired a small
software company specializing in optimization algorithms and scenario
management.  Marc holds graduate degrees in physics from the
University of Alaska Fairbanks and Otago University, New Zealand.
\end{IEEEbiography} 

\newpage

%% annex - bits of code, the actual real thing
\appendices
%\onecolumn
\section{Something interesting}
I'm not sure but perhaps an overview.

%% Implementation Details
\section{Implementation Details}

\subsection{Characteristics of PV Generation}


\subsection{Predictor using the past for the future}

\begin{code}
pastPredict s t = 10
\end{code}

\subsection{minRunTime}

MinRunTime is intended
\begin{code}
-- minRunTime period goes up instantly but remains at its level
--   until the timeout t expires
minRunTime t [] = []
minRunTime t (x:xs) = x:minRunTime_ t 1 x xs

minRunTime_ :: Int -> Int -> Double -> [Double] -> [Double]

minRunTime_ t c v [] = []
minRunTime_ t c v (x:xs) = 
 if c < t && x <= v -- no timeout, no step up so stay at v
 then v:minRunTime_ t (c+1) v xs
 else if c < t && x > v -- no timeout, increased so go up to x
 then x:minRunTime_ t (c+1) x xs
 else if c >= t && x <= v -- timedout, no step so goto v
 then x:minRunTime_ t 1 x xs
 else {- c >= t && x > v -} -- timedout, increased so goto x
      x:minRunTime_ t 1 x xs     


prop_minRunTime_ident_0 :: [Double] -> Bool
prop_minRunTime_ident_0 xs = xs == minRunTime 0 xs

prop_minRunTime_ident_1 :: [Double] -> Bool
prop_minRunTime_ident_1 xs = xs == minRunTime 1 xs

examples_minRunTime = do
 examples "examples_minRunTime.txt" display exprn isok tests
 where display ((t,xs),ok) = "minRunTime " ++ show t ++ " " ++ show xs
       exprn ((t,xs),ok) = minRunTime t xs
       isok ((t,xs),ok) = exprn((t,xs),ok) == ok 
       tests = [((0,[1..10]),[1..10]),
                ((0,[5,4..1]),[5,4..1]),
                ((1,[5,4..1]),[5,4..1]),
                ((5,[5,4..1]),[5,5,5,5,5]),
                ((2,[5,4..0]),[5,5,3,3,1,1]),
                ((2,[1..10]),[1..10]),
                ((3,[10,9..1]),[10,10,10,7,7,7,4,4,4,1])
               ]
       
\end{code}

hysterisisDown
\subsection{HysterisisDown}

\begin{code}
-- a signal must decrease by at least k before we accept it
hysterisisDown k [] = [] 
hysterisisDown k (x:xs) = x:hysterisisDown_ k x xs

hysterisisDown_ k v [] = []
hysterisisDown_ k v (x:xs) = 
 if x >= v 
 then x:hysterisisDown_ k x xs
 else if x <= v-k 
 then x:hysterisisDown_ k x xs
 else v:hysterisisDown_ k v xs


examples_hysterisisDown = do
 examples "examples_hysterisisDown.txt" display exprn isok tests
 where display ((t,xs),ok) = "hysterisisDown " ++ show t ++ " " ++ show xs
       exprn ((t,xs),ok) = hysterisisDown t xs
       isok ((t,xs),ok) = exprn((t,xs),ok) == ok 
       tests = [((0,[1..10]),[1..10]),
                ((0,[0,-24.0]),[0,-24.0]),
                ((0,[0,6.0]),[0,6.0]),
                ((10,[1..100]),[1..100]),
                ((10,[100,90..10]),[100,90..10])
               ]

prop_hysterisisDown_0 :: [Double] -> Bool
prop_hysterisisDown_0 xs = xs == hysterisisDown 0 xs

\end{code}

capacity
\subsection{Capacity}
\begin{code}
capacity :: [Double] -> [Double] -> [Double]
capacity caps xs = map (capacity_ caps maxcap) xs
 where maxcap = maximum caps

capacity_ :: [Double] -> Double -> Double -> Double
capacity_ [] maxcap x = maxcap -- exceed maximum capacity
capacity_ (cap:caps) maxcap x = 
 if x <= cap then cap
 else capacity_ caps maxcap x

capacityDefault = capacity [300,600..1000]
\end{code}


\subsection{Tools}

\begin{code}
noise :: (Random a) => Int -> [a]
noise seed = randoms (mkStdGen seed)

step01 :: Int -> Int -> [Double]
step01 p q = cycle l 
 where l = (replicate p 0.0) ++ (replicate q 1.0)

repeats xs p = cycle $ repeats_ xs p
repeats_ (x:xs) p = (replicate p x) ++ (repeats_ xs p)
repeats_ [] p = [] 

walk seed pu =
 intergrate sig
 where sig = scale pu $ offset (-0.5) $ noise seed
       
       offset k s = map (k+) s
scale k s  = map (k*) s
offset k s  = map (k+) s

diff :: [Double] -> [Double] -> [Double]
diff [] [] = []
diff (x:xs) (y:ys) = (x-y):diff xs ys
diff xs [] = xs
diff [] ys = ys

combine :: [Double] -> [Double] -> [Double]
combine [] [] = []
combine xs [] = xs
combine [] ys = ys
combine (x:xs) (y:ys) = (x+y: combine xs ys)

limitabove_s k s = map (max k) s
limitbelow_s k s = map (min k) s
limits low high s = map (limit low high) s
limit low high v = max low $ min high v

intergrate s = intergrate' 0.0 s
intergrate' p (x:xs) = (p+x):(intergrate' (p+x) xs)
intergrate' s [] = []

intergrate_within v low high = intergrate_within_ 0.0 v low high
intergrate_within_ s (x:xs) low high = sum:(intergrate_within_ sum xs low high)
 where sum = limit low high (x+s)
intergrate_within_ s [] low high = []

\end{code}

\subsection{\texttt{smooth} -- smoothing data}\label{smooth}

A common process in all control is the smoothing of data in 
order to eliminate:

\begin{enumerate}
\item Instrument noise.
\item Synchronisation noise where a signal is ...
\end{enumerate}

The \texttt{smooth} function implements a simple single pole recursive
low pass filter which behaves in the same way as a simple RC circuit.
See DSP guide *http://www.dspguide.com/ch18/2.htm* for an introduction
to these filters.

\begin{code}
smooth :: Double -> [Double] -> [Double]
smooth k xs = smooth_ xs (head xs) ((tau k))
smooth_ (x:xs) prev k = f:(smooth_ xs f k)
 where f = (k * x) + ((1 - k) * prev)
smooth_ [] prev k = []

tau t = if t > 0 then (1 - exp((-1)/t)) else 1.0

examples_smooth = do
 examples "examples_smooth.txt" display exprn isok tests
 where display ((a,b),ok) = "smooth " ++ show a ++ " " ++ show b
       exprn ((a,b),ok) = smooth a b
       isok ((a,b),ok) = exprn((a,b),ok) == ok 
       tests = [((0.0,[60..65]),[60..65])]
       
prop_smooth_ident_0 :: [Double] -> Bool
prop_smooth_ident_0 xs = (smooth 0 xs) ~==~ xs

prop_smooth_ident_1 :: Double -> Bool
prop_smooth_ident_1 x = (smooth 1 (replicate 100 x)) ~==~ (replicate 100 x)

prop_smooth_ident_2 :: Double -> Bool
prop_smooth_ident_2 x = (smooth 60 (replicate 100 x)) ~==~ (replicate 100 x)
 
prop_tau_limits :: Double -> Bool
prop_tau_limits k = 0 <= tau k && tau k <= 1
\end{code}

Examples for  
\begin{code}
examples_tau = do
 examples "examples_tau.txt" display exprn isok tests
 where display (a,ok) = "tau " ++ show a 
       exprn (a,ok) = tau a
       isok (a,ok) = (abs (exprn(a,ok) - ok)) < 0.01
       tests = [(0.0,1), -- straight thru
                (1.0,0.632), 
                (10.0,0.0951),
                (60,0.0165),
                (600.0,0.001)]
\end{code}

\begin{code}
figSmooth1 = fig "figSmooth1.pdf" 
   [figLine (impulse,"step", black),
    figLine (smooth 1 $ impulse,"t=1", red),
    figLine (smooth 10 $ impulse,"t=10", green),
    figLine (smooth 60 $ impulse,"t=60", blue),
    figLine (smooth 600 $ impulse,"t=600", brown)
    ]
   figCfg
 where impulse = take 110 $ step01 10 100
\end{code}
\begin{figure} 
\includegraphics[width=1.0\columnwidth]{figSmooth1.pdf}
 \caption{Smoothing}
 \label{figLoadP1}
\end{figure}

\subsection{Others}

\begin{code}
generate :: [Double] -> Vector Double
generate vs = fromList vs :: Vector Double
\end{code}

\subsection{\texttt{maxup}}

\begin{code}
maxup :: Int -> [Double] -> [Double]
maxup n [] = []
maxup n (x:xs) = v:(maxup n xs)
 where v = if xs == [] 
           then 0 
           else max 0 ((maximum $ take n xs)-x)
\end{code}

\begin{code}
time :: IO t -> IO t
time a = do
   start <- getCPUTime
   v <- a
   end   <- getCPUTime
   let diff = (fromIntegral (end - start)) / (10^12)
   printf "cputime = %0.3fs\n" (diff :: Double)
   return v
 
make t f = do
  s <- getArgs
  if makeOk t s
    then do
       putStr $ "MAKING " ++ t ++ "\t" 
       time f
       return 0
    else do
         -- putStrLn $ "SKIPPING " ++ t
         return 0
  return 0
  
makeOk t [] = False
makeOk t ["all"] = True
makeOk t (x:xs) = 
  if t == x || isInfixOf x t
  then True
  else makeOk t xs
\end{code}  
 
\begin{code}
prop_maxup_0 :: Int -> Property
prop_maxup_0 n = n > 0 ==> (maxup n [1,1,1,1]) == [0,0,0,0]

prop_maxup_1 :: Int -> Property
prop_maxup_1 n = n > 0 ==> (maxmaxup n [1,1,2,1]) == 1

prop_maxup_2 :: Int -> Property
prop_maxup_2 n = n > 0 ==> (maxmaxup n [1,1,0,1]) == 1

prop_maxup_3 :: Int -> Property
prop_maxup_3 n = n > 0 ==> (maxmaxup n [1,1,1,100]) == 99
\end{code}

\subsection{\texttt{maxmaxup}} 
\begin{code}
maxmaxup n [] = 0.0
maxmaxup n xs = maximum (maxup n xs)
\end{code}

\subsection{\texttt{maxdown}}

\begin{code}
maxdown :: Int -> [Double] -> [Double]
maxdown n [] = []
maxdown n (x:xs) = v:(maxdown n xs)
 where v = if xs == [] 
           then 0.0
           else abs (x - (minimum $ take n $ xs))
\end{code}

\subsection{\texttt{maxmaxdown}}

\begin{code}
maxmaxdown n [] = 0.0
maxmaxdown n xs = maximum (maxdown n xs)
\end{code}

\begin{code}
tselect tp ts xs = take (ceiling tp) $ drop (round ts) xs
tselect1d = tselect (1 * d)
tselect1w = tselect (1 * w)
tselect1m = tselect (1 * m)
\end{code}

\begin{code}
m = 60 :: Double
h = m * 60 :: Double
d = h * 24 :: Double
w = d * 7 :: Double
y = w * 52 :: Double
\end{code}

\subsection{\texttt{fig} -- fig drawing support}
The \texttt{fig} is used to generate PDF charts using
Vivian McPhails Plot package \cite{Vivian:Haskell:Plot}. 

\begin{code}
-- create a record to hold the configuration for a plot
data FigCfg =  
 FigCfg {
   plottitle :: String,
   xtitle :: String,
   xlow,xstep,xhigh :: Double,
   xfmt :: String,
   xticks :: Int, 
   xgrid :: Bool,
   ytitle :: String,
   ylow, yhigh :: Double,
   yfmt :: String,
   yticks :: Int,
   ygrid :: Bool
   }

-- and define a default one
figCfg = FigCfg {
              plottitle = "",
              xtitle = "t (h)",
              xlow = 0, 
              xstep = ((1.0)/(1.0 * h)), 
              xhigh = nhrs,
              xfmt = "%.0f", 
              xticks = 13,
              xgrid = False, 
              ytitle = "kW" ,
              yfmt = "%.0f",
              ylow = 0, yhigh = 600, 
              yticks = 13,
              ygrid = False
              }
\end{code}
 

\begin{code}
fig filename lines cfg = do
   let xv = fromList [xlow cfg,
                      xlow cfg + xstep cfg..xhigh cfg] :: Vector Double
   figWrite filename (figNew (xv,lines) cfg)
   figShow filename
\end{code}

\begin{code}
figLine (xs,s,c) = line ((generate xs), s) c
figPoint (xs,s,c) = point ((generate xs), s) (Bullet,c)

figLineStats (xs,s,c) = 
 line ((generate xs),  s ++ " " ++ figStats xs) c  

figWrite filename fig = do 
 writeFigure PDF filename (1600,800) fig
\end{code}

\begin{code}  
figNew ds cfg = do 
 figDefaults cfg
 withTitle $ setText $ plottitle cfg
 withPlot (1,1) $ do
   setDataset ds
   figX cfg
   figY cfg
   setRange YAxis Lower Linear (ylow cfg) (yhigh cfg)
   setRange XAxis Lower Linear (xlow cfg) (xhigh cfg)
   setLegend True NorthEast Outside

figShow filename = 
 print filename
 -- rawSystem "evince" ["-f", filename] 
 
figX cfg = do
 addAxis XAxis (Side Lower) $ do
   setGridlines Major $ xgrid cfg
   withAxisLabel $ setText $ xtitle cfg
   setTicks Major (TickNumber (xticks cfg))
   setTicks Minor (TickNumber 0)
   setTickLabelFormat $ Printf $ xfmt cfg

figY cfg = do
 addAxis YAxis (Side Lower) $ do
   setGridlines Major $ ygrid cfg
   withAxisLabel $ setText (ytitle cfg)
   setTicks Major (TickNumber (yticks cfg))
   setTicks Minor (TickNumber 0)
   setTickLabelFormat $ Printf $ yfmt cfg
   
figDefaults cfg = do 
 withTextDefaults $ setFontFamily "OpenSymbol"
 withTextDefaults $ setFontSize 28
 withLineDefaults $ setLineWidth 2
 withPointDefaults $ setPointSize 2
 setPlots 1 1 
\end{code}

\subsection{\texttt{figStats} - statistics in figs}

A simple statistics caculator that returns the 
\texttt{min..average..max} of a list for use in the figs legend.

\begin{code}
figStats :: [Double] -> String

figStats xs  = 
 "\n" ++ ls ++ ".." ++ ms ++ ".." ++ hs  
 where ls = printf "%.0f" $ l
       ms = printf "%.0f" $ m 
       hs = printf "%.0f" $ h
       l = minimum xs
       m = s/n
       h = maximum xs
       s = sum xs
       n = fromIntegral (length xs) :: Double
\end{code}

\subsection{\texttt{freq} - calculate frequency across a range}
Calculate the frequency of an value.

\begin{code}
freq low n high xs = freq_ low' step' high' xs
 where
   low' = min low $ minimum xs
   step' = (high' - low') / n
   high' = max high $ maximum xs

freq_ :: Double -> Double -> Double -> [Double] -> [(Double,Double,Int)]
freq_ low w high xs = 
 if low < high then r:rs
 else [] 
 where r = (low,(low+w),(length $ filter (between low (low+w)) xs))
       between l h v = l <= v && v < h
       rs = freq_ (low+w) w high xs
\end{code} 

\subsection{\texttt{runLengthCompress}}

Run length compression replaces identical sequences in a 
a list with a tuple containing the value and the number of 
identical values (the run length). This method is used for:

\begin{enumerate}
\item Compressing data for both reading and writing.
\item Writing ASIM compatible data sets.
\item Displaying data to humans.
\end{enumerate}

The \verb+runLengthCompress+ function takes a list of data 
at a given fixed time stamp (typically 1s) and returns the 
compressed information.

\begin{code}
runLengthCompress :: [Double] -> [(Double,Int)]

runLengthCompress [] = []
runLengthCompress (x:xs) = (x,cnt+1):(runLengthCompress $ drop cnt xs)
 where cnt = length $ takeWhile (x==) xs
\end{code}

The \verb+runLengthDecompress+ decompresses the data from
\verb+runLengthCompress+.

\begin{code}
runLengthDecompress :: [(Double,Int)] -> [Double]

runLengthDecompress [] = []
runLengthDecompress ((x,n):xs) = (replicate n x) ++ (runLengthDecompress xs)
\end{code}

The following examples may help:

% \VerbatimInput{examples_runLengthCompress.txt}

\begin{code} 
examples_runLengthCompress = do 
 examples "examples_runLengthCompress.txt" display exprn isok tests 
 where display (a,ok) = "runLengthCompress" ++ " " ++ (show a) 
       exprn (a, ok) = runLengthCompress a
       isok (a, ok) = exprn(a,ok) == ok
       tests = [(([]),[]),
                (([10]),[(10,1)]),
                (([10]),[(10,1)]),
                (([10, 10]),[(10,2)]),
                (([10, 8]),[(10,1),(8,1)]),
                ((replicate 10 13),[(13,10)]),
                (((replicate 10 13) ++ (replicate 4 3)), [(13,10),(3,4)])
               ]
\end{code}

The following properties hold for these functions:

\begin{code}
newtype SmallDoubleList = SmallDoubleList [Double] deriving (Eq,Show)

instance Arbitrary SmallDoubleList where
 arbitrary = sized $ \s -> do
                n <- choose (0,s `min` 100)
                xs <- vectorOf n (choose (0,10))
                let xs' = (map (significantDigits 0) xs)
                return (SmallDoubleList xs')
 shrink (SmallDoubleList xs) = map SmallDoubleList (shrink xs)
 
-- runLengthDecompress is the inverse of runLengthCompress
prop_runLength_ident :: SmallDoubleList -> Bool
prop_runLength_ident (SmallDoubleList xs) = 
 xs == (runLengthDecompress $ runLengthCompress xs)

-- compression never increases the size of the data 
prop_runLength_size :: SmallDoubleList -> Bool
prop_runLength_size (SmallDoubleList xs) = 
 (length $ runLengthCompress xs) <= length xs

prop_runLength_max :: SmallDoubleList -> Bool
prop_runLength_max (SmallDoubleList xs) = 
 xs == [] || (maximum xs) == (maximum $ fst $ unzip $ runLengthCompress xs) 

prop_runLength_min :: SmallDoubleList -> Bool
prop_runLength_min (SmallDoubleList xs) = 
 xs == [] || (maximum xs) == (maximum $ fst $ unzip $ runLengthCompress xs) 

prop_runLength_len :: SmallDoubleList -> Bool
prop_runLength_len (SmallDoubleList xs) = 
 (length xs) == (sum $ snd $ unzip $ runLengthCompress xs) 
\end{code}

\begin{code}
showRunLengths xs = showRunLengths_ 0 $ runLengthCompress xs

showRunLengths_ :: Double -> [(Double,Int)] -> String
showRunLengths_ t [] = ""
showRunLengths_ t ((a,b):xs) = 
 (showTime t) ++ "\t" ++ (showTime (fromIntegral b)) ++ 
 "\t" ++ (show a) ++ "\n" ++
 showRunLengths_ (t+(fromIntegral b)) xs
 where now = t + (fromIntegral b)
   
showTime at = printf "%6d:%02d:%02d" (h::Int) (m::Int) (s::Int)
 where s = round(at) `mod` 60
       m = round(at/60) `mod` 60
       h = round(at/3600) 
\end{code}

\subsection{\texttt{compressDeadband} - deadband compression}

\begin{code}
{- ttt implement
compressDeadband :: Double -> Double -> [(Double,Int)] -> [(Double,Int)]
compressDeadband vdb zdb [] = []
compressDeadband vdb zdb xs = compressDeadband_ vdb zdb xs

compressDeadband_ vdb zdb [] = []
compressDeadband_ vdb zdb ((v,n):xs) = 
-}   
\end{code}
\subsection{\texttt{significantDigits}}

\begin{code}
significantDigits :: Int -> Double -> Double
significantDigits n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

prop_significantDigits_0 :: Int -> Bool
prop_significantDigits_0 v =
 (significantDigits 0 (fromIntegral v)) == (fromIntegral v)

-- ttt review this code and its necessity
\end{code}

\subsection{\texttt{percent} - format a percentage}

\begin{code}
percent a b = 100 * (a/b)
showPercent a b = printf "%.1f" $ percent a b

percentChange a b = (percent a b) - 100
showPercentChange a b = printf "%.1f" $ percentChange a b
\end{code}

\subsection{Approximate equality}

Much of the code above can introduce approximation errors so two
operators are defined for approximately equal for \texttt{Double} and
\texttt{[Double]}.

\begin{code}
(~==) :: Double -> Double -> Bool
a ~== b = abs(a-b) < 0.0000001

(~==~) :: [Double] -> [Double] -> Bool

[] ~==~ [] = True
(a:as) ~==~ [] = False
[] ~==~ (b:bs) = False
(a:as) ~==~ (b:bs) = a ~== b && as ~==~ bs
\end{code}

Testing support 
\section{Testing}
\subsection{\texttt{check} quickCheck property based testing}

The quickCheck library \cite{Cleasessen:2011:QLT} 
provides property based checking which
automatically generates test cases and checks them.against 
propertiers.
All properties begin with \texttt{prop\_} and are
automatically checked. 

An example of a property might be that for any list, two \texttt{reverse} 
operations on a list return the original list. 

\begin{code}
prop_reverse_0 :: [Double] -> Bool 
prop_reverse_0 xs = reverse (reverse xs) ==  xs
\end{code}

In this program all properties will be checked when the command line
argument \texttt{check} or \texttt{all} is passed to it/

\begin{code}
check = do -- check all prop_* in this program 
 putStrLn ""
 $(quickCheckAll) >>= \passed -> 
   if passed then checkOk
   else checkFailed
 
verbosecheck = do -- verbose check all prop_* in this program 
 putStrLn ""
 $(verboseCheckAll) >>= \passed -> 
   if passed then checkOk
   else checkFailed

checkOk = putStrLn "check: All tests passed."

checkFailed = do -- on failure run a verbose check and fail!
 $(verboseCheckAll)
 error "check: fatal error some checks failed"
\end{code}

\subsection{\texttt{Examples} - generating example output}

The \texttt{examples} function generates a file of example outputs
for use as both examples and tests. The \texttt{file} is the result of
each function.

\begin{code}                            
examples file display exprn isok xs = do
 let s = unlines $ map (example display exprn isok) xs
 putStr s
 writeFile file s

example display exprn isok v = 
 "example " ++ (display v) ++ 
 " ->\n\t" ++ (show result) ++ " " ++ 
 (if isok v then "OK" else "FAILED")
 where result = exprn v
\end{code}

Haskell and Emacs
\section{Haskell, Literate Programming and Emacs}
\subsection{Haskell}
Haskell is a lazy functional lanuage which 
a number of benefits..
\begin{itemize}
\item Variables in Haskell do not vary
\item A function will always return the same result if passed the same input - no exceptions.
\item Functional programs tend to be shorter (usually between 2 to 10 times shorter).
\end{itemize}

%% Literate :bit - a description of the literate haskell
\subsection{Literate Programming}
Add in an intro to literate programming.

The main.lhs is a LaTeX file with Haskell  
code fragments delimited by \verb+\begin{code}+ and \verb+\end{code}+

Individual components can be included or excluded using the comment 
package which is configured at the top of \verb+main.lhs+.

Each subsection is typically laid out as:

\begin{verbatim}
%% pv:bit - description about PV
\begin{pv:bit}
\subsection{PV is interesting}
...
\end{pv:bit}
\end{verbatim}

%% Emacs usage
\subsection{Emacs}
The emacs setup uses orgstruct minor mode which is part of
org mode and provides outlining. For the setup below:

\begin{verbatim}
%% Top level
%%% Next one done
%%% And another
%% Another Top level
\end{verbatim}

The emacs setup file (\verb+.emacs+) needs to include something akin to:

\begin{verbatim}
;; org mode setup
(add-to-list 'load-path "~/imports/org-8.2.6/lisp")
(defun my-org-latex-hook () (interactive)
 (orgstruct-mode)
;;  (orgstruct++-mode) -- evil breaks things
 (setq org-outline-regexp "^%%+ ")
 (setq org-outline-heading-regexp "^%%+ .*$")
 (setq orgstruct-heading-prefix-regexp "^%%+ ")
 (local-set-key [M-tab]
		 (lambda () (interactive) (org-cycle t)))
 (local-set-key [tab]
		 (lambda () (interactive) (org-cycle))))
(add-hook 'TeX-mode-hook 'my-org-latex-hook)

; mode for main.lhs
(defun my-lhs-mode () (interactive)
 (literate-haskell-mode)
 (my-org-latex-hook))

; hook to activiate it
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . my-lhs-mode))
\end{verbatim}

%% main program
\subsection{Main Program}
\begin{code}
main = do
 make "check" check 
 -- make "figKalkPvAvailP.pdf" figKalkPvAvailP
 -- print $ events sKalkPvAvailP
 -- make "verbosecheck" verbosecheck 
 make "figPvAvailP1" figPvAvailP1
 make "figPvAvailMaxDown120P1" figPvAvailMaxDown120P1
 make "figPvAvailMaxDown120PAboveSpin1" figPvAvailMaxDown120PAboveSpin1
 -- make "examples_fig" examples_fig
 -- make "examples_runLengthCompress" examples_runLengthCompress
 -- make "figLoad1P" figLoad1P
 -- make "figLoad2P" figLoad2P
 -- make "figLoad3P" figLoad3P
 -- make "figLoad4P" figLoad4P
 make "figFuelCurve1" figFuelCurve1
 make "figFuelCurve2" figFuelCurve2
 make "figFuelCurve3" figFuelCurve3
 make "figFuelCurve4" figFuelCurve4
 make "figFuelCurve5" figFuelCurve5
 -- make "figPv1" figPv1
 -- make "figPv2" figPv2
 -- make "figPerfect1" figPerfect1
 -- make "figPerfect2" figPerfect2
 -- make "putPerfect" putPerfect
 -- make "figSimplicitySpin" figSimplicitySpin
 -- make "figSimplicitySmooth" figSimplicitySmooth
 -- make "figSimplicityHyst" figSimplicityHyst
 -- make "figSimplicityCap" figSimplicityCap
 -- make "figSimplicityMinRun" figSimplicityMinRun
 -- make "figSimplicity30P" figSimplicity30P
 -- make "figPredictor30mP" figPredictor30mP
 -- make "figCompare1" figCompare1
 -- make "figCompare2" figCompare2
 make "figSmooth" figSmooth1
 make "examples_fuelUsed" examples_fuelUsed
 make "examples_tau" examples_tau
 make "examples_smooth" examples_smooth
 -- make "examples_minRunTime" examples_minRunTime
 -- make "examples_hysterisisDown" examples_hysterisisDown
 -- make "fuelImprove" fuelImprove
 -- make "fuelImprove2" fuelImprove2 -- disabled for now
 -- write1s "tt.e1s" sLoadMaxUp60sP
 return 0
\end{code}

\subsection{Time series IO support}
Hello
\subsection{\texttt{read1s} -- read 1s data}

The \texttt{read1s} data is used to read data at 1s time steps in 
plain old ascii from a file. These files are typically generated
by other tools such as SCADA or historians such as PI.

\begin{code}
read1s filename  = unsafePerformIO $ read1s_ filename
read1s_ :: FilePath -> IO [Double]
read1s_ filename = do
 f <- readFile filename
 let l = lines f
 let s = map (read :: String -> Double) l
 return s
\end{code}
 
\subsection{\texttt{write1s} -- write 1s to a file}

Write a 1s data set from \texttt{xs} into filename \texttt{filename}.
This function is typically used for debugging.

\begin{code}
write1s filename xs = do
 let ls = map (show :: Double -> String) xs
 writeFile filename $ unlines ls
\end{code}


%% 
\section{Bits of info}

\subsection{Colins Comments}
{\small
\begin{verbatim}
From:	Colin Bonner <c.bonner@fulcrum3d.com>
Sent:	Friday, 20 March 2015 3:29 PM
To:	Maker, Phillip; tom.loveard@gmail.com; antonin braun 
(braun.antonin@gmail.com)
Subject:	Re: Record of conversation

Hi Gents,

Great to touch base with everyone today.

I'd like to comment on the data analysis and thought an email would be more comprehensible. 

Regardless of the comparative metric, we are aiming for a robust statistical outcome. 
IMO this means we need a specific number of data points instead of a time frame. This is done 
in wind energy industry when validating  remote sensing devices against mast: typically, a 
minimum period of 3 months is required AND 6 data points in each range bin wind speed range 
bin between 4.0~4.5, 4.5~5.0, ..., 15.5~16.0m/s. The minimum of 6 data points does give poor 
higher order stats and is quite commonly observed at the higher wind speeds, but it's the 
reality of not being able to test forever...

I'd like to propose the tests run for a minimum of 3 months  AND until N events are observed by 
PWC's system OR 6 months occurs. This does introduce the issue of defining the "events". First 
thoughts would to be define something like:
1) Event A: drop in PV that lasts < 30 sec -- this is interesting form a bat/fly wheel perspective.
2) Event B: drop in PV that lasts >= 30sec & < 2 min -- just interesting?
3) Event C: drop in PV that lasts > 2 min -- interesting for spinning reserve management.  

Drops in PV would be defined as per tender RFQ.  Might only need 2 events, or define events in 
terms of Energy (power loss x time)... I'd like to see N be 500~1000. Phil probably has history 
data from Bullman to see how realistic this is. F3D's work further south suggests this could 
happen in a few months.  

Regarding the analysis for the paper, I think it's essential to do the method 2 Phil drafted. 
The reality is that correct and incorrect predictions do not have the same weight for hybrid 
grids when grid stability is included in the analysis (more so on high pen). 
I know AEMO/ANU/NICTA/CSIRO are actively researching how to define the "cost" of errors in 
med/high penetration of PV for their machine learning algo for solar forecasting for this very 
reason (They are looking at sky imagery, sat imag and distributed irradiance 
sensors). AFAIK they are heading towards a sqrd-error + penalty. A financial analysis with 
a penalty for missed predictions (similar to PPA's for bat storage systems) would be very 
interesting. 

I'm putting this out there for discussion. 

Cheers,
Colin

Colin Bonner  
Director



SODAR | RESOURCE MONITORING | NOISE MONITORING | 
CLOUD TRACKING
Level 11, 75 Miller St, NORTH SYDNEY, NSW 2060 AUSTRALIA
M +61 414 785 489 | info@fulcrum3D.com | www.fulcrum3D.com

From: Maker, Phillip <Phillip.Maker@powerwater.com.au> 
Sent: Friday, 20 March 2015 4:37 PM 
To: Colin Bonner; tom.loveard@gmail.com; antonin braun (braun.antonin@gmail.com) 
Subject: Record of conversation 

Gentlemen,

Thanks for the meeting, I hope it was worthwhile, any suggestions taken on board.
Here is my summary of the meeting:

1.       Agreement in Principle on things in the kickoff document (at least for now).
2.       Ill rewrite it a bit next week and forward it to all parties for signoff/ack.
3.       Kit delivery times: could everyone just confirm these timings next but my notes
Indicate:
a.       Colin  3rd week APR, 2 week testing in Darwin
b.      Antonin  3rd week MAY, 2 weeks testing in Darwin 
c.       Tom  fine with APR, 2 weeks testing.
4.       PWC will assist in the testing as your kit arrives, particularly the MODBUS stuff.
a.       Im guessing we do a formal test of everything MODBUS in Darwin
b.      And perhaps all the update/download procedures etc.
c.       Well provide a Telstra 3G network connection for you dial in on.
5.       Re mounting: PWC to provide a drawing (these are a bit mysterious sometimes
In the north but Ill give it a go).
6.       In general well be responsible for the physical install with your advice. If we need to 
Buy a few poles/brackets that is all with us. (including cost).
7.       Phil to ring atonin re MODBUS stuff
8.       Princples/Paper due next week for initial review

Anything else?

Ta
\end{verbatim}
}

%% testpattern
\begin{testpattern:bit}
\newpage
\section{Test pattern}

This section is just a test pattern to ensure that the code will fit
within the bounds of the paper. For this purpose we use 72 characters 
as the limit. It should definitely NOT appear in the final version.
\begin{code}
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
-- 3456789012345678901234567890123456789012345678901234567890123456789012
--        10        20        30        40        50        60        70
\end{code}
\end{testpattern:bit}

\end{document}

